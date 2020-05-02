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
    BankAccounts,
    configToken,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time
import GHC.Generics
import Types
import Prelude

type BankAccounts = Map.Map Int LastImported

data Config
  = Config
      { _bankAccounts :: BankAccounts,
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

configToken :: Config -> UTCTime -> Either InvalidToken ValidToken
configToken config currentTime =
  case config ^. token of
    Just t ->
      case config ^. tokenExpiresAt of
        Just expires -> do
          if expires <= currentTime
            then Left $ InvalidToken Expired
            else Right $ ValidToken $ BS.pack t
        Nothing -> Left $ InvalidToken Missing
    Nothing -> Left $ InvalidToken Missing
