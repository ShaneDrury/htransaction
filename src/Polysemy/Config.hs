{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Config
  ( runGetConfig,
    runGetConfigCached,
    runGetConfigTest,
    runWriteConfig,
  )
where

import Config
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map
import Data.Time
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace
import Types
import Prelude

runGetConfig :: (Members '[Trace, Embed IO] r) => FilePath -> Sem (Input Config ': r) a -> Sem r a
runGetConfig fp = interpret $ \case
  Input -> do
    trace $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runWriteConfig :: (Members '[Trace, Embed IO] r) => FilePath -> Sem (Output Config ': r) a -> Sem r a
runWriteConfig fp = interpret $ \case
  Output cfg -> do
    trace $ "Writing config to " ++ fp
    embed $ S.writeFile fp (encode cfg)

data Cached a = Cached a | Dirty deriving (Eq, Show)

runGetConfigCached :: Members '[Input Config, Output Config] r => Sem (State (Cached Config) : r) a -> Sem r a
runGetConfigCached =
  evalState Dirty
    . intercept @(Input Config)
      ( \case
          Input -> do
            cached <- get @(Cached Config)
            case cached of
              Cached cfg -> return cfg
              Dirty -> do
                v <- input
                put $ Cached v
                return v
      )
    . intercept @(Output Config)
      ( \case
          Output v -> do
            cached <- get @(Cached Config)
            case cached of
              Cached old ->
                when (old /= v) $ do
                  output v
                  put $ Cached v
              Dirty -> do
                output v
                put $ Cached v
      )

runGetConfigTest :: Sem (Input Config ': r) a -> Sem r a
runGetConfigTest = interpret $ \case
  Input ->
    return $
      Config
        { _bankAccounts = Map.fromList [(123, LastImported $ fromGregorian 2020 02 04), (679673, LastImported $ fromGregorian 2020 02 07)],
          _token = Just "token",
          _refreshToken = Just "refresh",
          _authorizationEndpoint = "https://api.freeagent.com/v2/approve_app",
          _tokenEndpoint = "https://api.freeagent.com/v2/token_endpoint",
          _clientID = "clientid",
          _clientSecret = "secret",
          _tokenExpiresAt = Nothing
        }
