{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( LastImported (..),
    handleErrors,
    AppError (..),
    ApiError (..),
    InvalidTokenReason (..),
    Transaction (..),
    TransactionDate (..),
    AccessToken (..),
    RefreshToken (..),
    AuthorizationCode (..),
    TokenEndpoint (..),
    BankInstitution (..),
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as CSV
import Data.Text
import Data.Time
import Database.Esqueleto
import Database.Persist.TH
import GHC.Generics
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Error
import Prelude hiding (log)

newtype LastImported = LastImported Day
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (PersistField, PersistFieldSql) via Day

newtype ApiError = Unauthorized H.HttpException deriving stock (Show)

data InvalidTokenReason = Expired deriving stock (Eq, Show)

data AppError
  = HttpError H.HttpException
  | AppApiError ApiError
  deriving stock (Show)

handleErrors :: Sem (Error H.HttpException : Error ApiError : Error AppError : r) a -> Sem r (Either AppError a)
handleErrors =
  runError @AppError
    . mapError AppApiError
    . mapError HttpError

newtype TransactionDate = TransactionDate Day
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance CSV.ToField TransactionDate where
  toField (TransactionDate d) = BS.pack $ show d

data Transaction = Transaction
  { dated_on :: TransactionDate,
    description :: Text,
    amount :: Text,
    comment :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, CSV.ToRecord, ToJSON)

newtype AccessToken = AccessToken Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (PersistField, PersistFieldSql) via Text

newtype RefreshToken = RefreshToken Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (PersistField, PersistFieldSql) via Text

newtype AuthorizationCode = AuthorizationCode String deriving stock (Eq, Show)

data TokenEndpoint = TokenEndpoint
  { access_token :: AccessToken,
    token_type :: String,
    expires_in :: Integer,
    refresh_token :: RefreshToken,
    refresh_token_expires_in :: Maybe Integer
  }
  deriving stock (Eq, Show)
  deriving anyclass (FromJSON)
  deriving stock (Generic)

data BankInstitution = Fa | Monzo
  deriving stock (Eq, Show, Generic, Ord, Read)
  deriving anyclass (FromJSON, ToJSON)

derivePersistField "BankInstitution"

instance ToJSONKey BankInstitution where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey BankInstitution where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
