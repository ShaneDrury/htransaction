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
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as CSV
import Data.Text
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Error
import Prelude hiding (log)

newtype LastImported = LastImported Day
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiError = Unauthorized deriving stock (Eq, Show)

data InvalidTokenReason = Expired | Missing deriving stock (Eq, Show)

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

newtype RefreshToken = RefreshToken Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
