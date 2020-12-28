module Types
  ( LastImported (..),
    handleErrors,
    AppError (..),
    ApiError (..),
    ValidToken (..),
    InvalidTokenReason (..),
    Transaction (..),
    TransactionDate (..),
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as CSV
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

newtype ValidToken = ValidToken BS.ByteString deriving stock (Eq, Show)

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
  toField (TransactionDate d) = BS.pack $ showGregorian d

data Transaction
  = Transaction
      { dated_on :: TransactionDate,
        description :: String,
        amount :: String
      }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, CSV.ToRecord, ToJSON)
