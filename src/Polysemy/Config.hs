{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Config
  ( runApiTokenM,
    runGetToken,
    runValidToken,
    saveTokens,
    runGetConfig,
    runWriteConfig,
    runWriteTokens,
  )
where

import Cli
import Config
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import Logger
import Network.HTTP.Req
import Polysemy
import Polysemy.BankAccount
import Polysemy.Input
import Polysemy.Output
import Polysemy.Random
import Polysemy.State
import Token
import Types
import Prelude

runGetConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Input Config) r
runGetConfig fp = interpret $ \case
  Input -> do
    info $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runWriteTokens :: (Members '[Logger, Embed IO, BankAccountsM] r) => FilePath -> InterpreterFor (Output Tokens) r
runWriteTokens fp = interpret $ \case
  Output cfg -> do
    info $ "Writing tokens to " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict @TokensConfig fp
    bankAccount <- getBankAccount
    case ecfg of
      Left e -> error e
      Right (TokensConfig tokensCfg) -> embed $ S.writeFile fp (encode @TokensConfig $ TokensConfig $ Map.insert (bankAccount ^. bankInstitution) cfg tokensCfg)

runWriteConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Output Config) r
runWriteConfig fp = interpret $ \case
  Output cfg -> do
    info $ "Writing config to " ++ fp
    embed $ S.writeFile fp (encode cfg)

runValidToken :: (Members '[ApiTokenM, TokenM, State Tokens, Input UTCTime] r) => InterpreterFor ValidTokenM r
runValidToken = interpret $ \case
  GetValidToken -> do
    eValidToken <- getToken
    case eValidToken of
      Left Missing -> toValidToken <$> getAccessToken
      Left Expired -> toValidToken <$> getRefreshToken
      Right v -> return v
  InvalidateTokens -> do
    oldTokens <- get @Tokens
    currentTime <- input
    put (oldTokens & tokenExpiresAt ?~ currentTime)

institutionEndpoint :: BankInstitution -> Url 'Https
institutionEndpoint institution = case institution of
  Fa -> https "api.freeagent.com" /: "v2" /: "token_endpoint"
  Monzo -> https "auth.monzo.com/"

runApiTokenM :: (Members '[Embed IO, State Tokens, Logger, BankAccountsM, RandomM] r) => InterpreterFor ApiTokenM r
runApiTokenM = interpret $ \case
  GetRefreshToken -> do
    config <- get @Tokens
    institution <- _bankInstitution <$> getBankAccount
    warn "Trying to refresh tokens"
    embed $ useRefreshToken (institutionEndpoint institution) (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))
  GetAccessToken -> do
    config <- get @Tokens
    institution <- _bankInstitution <$> getBankAccount
    case institution of
      Fa -> embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
      Monzo -> do
        state <- randomString 30
        embed $ putStrLn $ "Open and copy code: " <> monzoAuthUrl (config ^. clientID) state
    authorizationCode <- embed getLine
    embed $ getAccessTokenNetwork (institutionEndpoint institution) (config ^. clientID) (config ^. clientSecret) authorizationCode

doSaveTokens :: (Members '[Input UTCTime, State Tokens, ApiTokenM] r) => TokenEndpoint -> Sem r TokenEndpoint
doSaveTokens tokens = do
  originalConfig <- get @Tokens
  currentTime <- input @UTCTime
  put (withNewTokens tokens originalConfig currentTime)
  return tokens

saveTokens :: (Members '[Input UTCTime, State Tokens, ApiTokenM] r) => Sem r a -> Sem r a
saveTokens = intercept @ApiTokenM $ \case
  GetRefreshToken -> getRefreshToken >>= doSaveTokens
  GetAccessToken -> getAccessToken >>= doSaveTokens
