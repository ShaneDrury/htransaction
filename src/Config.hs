{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Time
import Data.Time.Clock
import GHC.Generics
import Polysemy
import Polysemy.Input
import Types

data Config
  = Config
      { _bankAccounts :: Map.Map Int LastImported,
        _token :: Maybe String,
        _refreshToken :: Maybe String,
        _authorizationEndpoint :: String,
        _tokenEndpoint :: String,
        _clientID :: String,
        _clientSecret :: String,
        _tokenExpiresAt :: Maybe UTCTime
      }
  deriving (Eq, Generic, Show)

$(makeLenses ''Config)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)

updateConfig :: Int -> LastImported -> Config -> Config
updateConfig bankAccount day = over bankAccounts (Map.insert bankAccount day)

runGetConfig :: (Members '[Embed IO] r) => FilePath -> Sem (Input Config ': r) a -> Sem r a
runGetConfig fp = interpret $ \case
  Input -> do
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

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
