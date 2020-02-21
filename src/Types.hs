{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as CSV
import Data.Time
import Data.Time.Clock
import GHC.Generics

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Message = Message String deriving (Eq, Show)

data Transaction
  = Transaction
      { dated_on :: Day,
        description :: String,
        amount :: String
      }
  deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving (Eq, Generic, Show, FromJSON)

newtype ValidToken = ValidToken BS.ByteString

data Refresh = Refresh

data AccessToken = AccessToken

data TokenEndpoint
  = TokenEndpoint
      { access_token :: String,
        token_type :: String,
        expires_in :: Integer,
        refresh_token :: Maybe String
      }
  deriving (Eq, Generic, Show, FromJSON)