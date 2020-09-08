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
    getConfig,
    writeConfig,
    ConfigM,
    BankAccountsM,
    getBankAccounts,
    runBankAccountsMOnConfig,
    runTokensM,
    runConfigM,
    runStateCached,
    Cached (..),
    runWriteTokens,
    TokensM (..),
  )
where

import Config
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map
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

data TokensM m a where
  GetTokens :: TokensM m Tokens
  WriteTokens :: Tokens -> TokensM m ()

data ConfigM m a where
  GetConfig :: ConfigM m Config
  WriteConfig :: Config -> ConfigM m ()

$(makeSem ''TokensM)

$(makeSem ''ConfigM)

runTokensM :: Sem (TokensM : r) a -> Sem (State Tokens : r) a
runTokensM = reinterpret $ \case
  GetTokens -> get @Tokens
  WriteTokens s -> put @Tokens s

runConfigM :: Sem (ConfigM : r) a -> Sem (State Config : r) a
runConfigM = reinterpret $ \case
  GetConfig -> get @Config
  WriteConfig s -> put @Config s

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
  GetBankAccounts :: BankAccountsM m (Map.Map Int BankAccount)

$(makeSem ''BankAccountsM)

runBankAccountsMOnConfig :: (Members '[ConfigM] r) => InterpreterFor BankAccountsM r
runBankAccountsMOnConfig = interpret $ \case
  GetBankAccounts -> do
    config <- getConfig
    let baList = config ^. bankAccounts
    return $ Map.fromList $ (\ba -> (ba ^. bankAccountId, ba)) <$> baList

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

runGetToken :: Members '[Input UTCTime, TokensM] r => InterpreterFor TokenM r
runGetToken = interpret $ \case
  GetToken -> configToken <$> getTokens <*> input @UTCTime

runValidToken :: (Members '[ApiTokenM, TokenM, TokensM, Input UTCTime] r) => InterpreterFor ValidTokenM r
runValidToken = interpret $ \case
  GetValidToken -> do
    eValidToken <- getToken
    case eValidToken of
      Left Missing -> toValidToken <$> getAccessToken
      Left Expired -> toValidToken <$> getRefreshToken
      Right (ValidToken validToken) -> return $ ValidToken validToken
  InvalidateTokens -> do
    oldTokens <- getTokens
    currentTime <- input
    writeTokens (oldTokens & tokenExpiresAt ?~ currentTime)

runApiTokenM :: (Members '[Embed IO, TokensM, Logger] r) => InterpreterFor ApiTokenM r
runApiTokenM = interpret $ \case
  GetRefreshToken -> do
    config <- getTokens
    warn "Trying to refresh tokens"
    embed $ useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))
  GetAccessToken -> do
    config <- getTokens
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    embed $ getAccessTokenNetwork (config ^. clientID) (config ^. clientSecret) authorizationCode

doSaveTokens :: (Members '[Input UTCTime, TokensM, ApiTokenM] r) => TokenEndpoint -> Sem r TokenEndpoint
doSaveTokens tokens = do
  originalConfig <- getTokens
  currentTime <- input @UTCTime
  writeTokens (withNewTokens tokens originalConfig currentTime)
  return tokens

saveTokens :: (Members '[Input UTCTime, TokensM, ApiTokenM] r) => Sem r a -> Sem r a
saveTokens = intercept @ApiTokenM $ \case
  GetRefreshToken -> getRefreshToken >>= doSaveTokens
  GetAccessToken -> getAccessToken >>= doSaveTokens
