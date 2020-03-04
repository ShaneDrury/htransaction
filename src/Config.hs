{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Config
  ( Config (..),
    clientID,
    clientSecret,
    token,
    tokenExpiresAt,
    refreshToken,
    updateConfig,
    bankAccounts,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Time
import GHC.Generics
import Types
import Prelude

data Config
  = Config
      { _bankAccounts :: Map.Map Int LastImported,
        _token :: Maybe String,
        _refreshToken :: Maybe String,
        _clientID :: String,
        _clientSecret :: String,
        _tokenExpiresAt :: Maybe UTCTime
      }
  deriving (Eq, Generic, Show)

$(makeLenses ''Config)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)

updateConfig :: Int -> LastImported -> Config -> Config
updateConfig bankAccount day = over bankAccounts (Map.insert bankAccount day)
