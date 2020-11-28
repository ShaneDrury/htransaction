{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Config
  ( runApiTokenM,
    runGetToken,
    runValidToken,
    saveTokens,
    runGetConfig,
    runWriteConfig,
    BankAccountsM (..),
    getBankAccount,
    runBankAccountsMOnConfig,
    runStateCached,
    Cached (..),
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
import Data.Maybe
import Data.Time
import Logger
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Token
import Types
import Prelude

data Cached a = Cached a | Dirty
  deriving stock (Eq, Ord, Show, Functor)

runStateCached :: forall v r a. Sem (State v : r) a -> Sem (Output v : Input v : r) a
runStateCached =
  evalState @(Cached v) Dirty
    . reinterpret3
      ( \case
          Get -> do
            cachedConfig <- get @(Cached v)
            case cachedConfig of
              Dirty -> do
                cfg <- input @v
                put @(Cached v) (Cached cfg)
                return cfg
              Cached cfg -> return cfg
          Put cfg -> do
            output @v cfg
            put @(Cached v) (Cached cfg)
      )

data BankAccountsM m a where
  GetBankAccount :: BankAccountsM m BankAccount

$(makeSem ''BankAccountsM)

runBankAccountsMOnConfig :: (Members '[State Config, Input Args] r) => InterpreterFor BankAccountsM r
runBankAccountsMOnConfig = interpret $ \case
  GetBankAccount -> do
    config <- get @Config
    args <- input @Args
    let baList = config ^. bankAccounts
    return $ fromJust $ find (\ba -> ba ^. Config.bankAccountId == Cli.bankAccountId args) baList

runGetConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Input Config) r
runGetConfig fp = interpret $ \case
  Input -> do
    info $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runWriteTokens :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Output Tokens) r
runWriteTokens fp = interpret $ \case
  Output cfg -> do
    info $ "Writing tokens to " ++ fp
    embed $ S.writeFile fp (encode cfg)

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
      Right (ValidToken validToken) -> return $ ValidToken validToken
  InvalidateTokens -> do
    oldTokens <- get @Tokens
    currentTime <- input
    put (oldTokens & tokenExpiresAt ?~ currentTime)

runApiTokenM :: (Members '[Embed IO, State Tokens, Logger] r) => InterpreterFor ApiTokenM r
runApiTokenM = interpret $ \case
  GetRefreshToken -> do
    config <- get @Tokens
    warn "Trying to refresh tokens"
    embed $ useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))
  GetAccessToken -> do
    config <- get @Tokens
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    embed $ getAccessTokenNetwork (config ^. clientID) (config ^. clientSecret) authorizationCode

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
