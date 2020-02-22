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

runGetConfig :: (Members '[Embed IO] r) => FilePath -> Sem (Input Config ': r) a -> Sem r a
runGetConfig fp = interpret $ \case
  Input -> do
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

data Cached a = Cached a | Dirty deriving (Eq, Show)

runGetConfigCached :: (Members '[Embed IO, Trace] r) => FilePath -> Sem (Output Config : Input Config : State (Cached Config) : r) a -> Sem r a
runGetConfigCached fp =
  evalState Dirty
    . interpret
      ( \case
          Input -> do
            cached <- get @(Cached Config)
            case cached of
              Cached cfg -> return cfg
              Dirty -> do
                trace "Getting config from file"
                ecfg <- embed $ eitherDecodeFileStrict fp
                case ecfg of
                  Left e -> error e
                  Right cfg -> do
                    put $ Cached cfg
                    return cfg
      )
    . interpret
      ( \case
          Output cfg -> do
            cCfg <- get @(Cached Config)
            case cCfg of
              Cached oldCfg ->
                when (oldCfg /= cfg) $ do
                  trace $ "Writing new config to " ++ fp
                  embed $ S.writeFile fp (encode cfg)
                  put $ Cached cfg
              Dirty -> do
                trace $ "Writing initial config to " ++ fp
                embed $ S.writeFile fp (encode cfg)
                put $ Cached cfg
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
