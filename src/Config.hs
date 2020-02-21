{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Time
import Data.Time.Clock
import GHC.Generics
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
