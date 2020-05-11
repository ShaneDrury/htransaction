{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Types
  ( runOutputOnLog,
    LastImported (..),
    handleErrors,
    AppError (..),
    LogMsg,
    info,
    Logger,
    runLoggerAsOutput,
    runLoggerOnRainbow,
    warn,
    err,
    LogType (..),
    ApiError (..),
    InvalidToken (..),
    ValidToken (..),
    InvalidTokenReason (..),
    Transaction (..),
    TransactionsEndpoint (..),
    TransactionDate (..),
  )
where

import Colog.Polysemy.Effect
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Trace
import Rainbow
import Prelude hiding (log)
import qualified Data.Csv as CSV
import Data.Coerce

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ApiError = Unauthorized deriving stock (Eq, Show)

data InvalidTokenReason = Expired | Missing deriving stock (Eq, Show)

newtype InvalidToken = InvalidToken InvalidTokenReason deriving stock (Eq, Show)
newtype ValidToken = ValidToken BS.ByteString deriving stock (Eq, Show)

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> InterpreterFor Trace r
runOutputOnLog verbose = interpret $ \case
  Trace msg -> embed $ when verbose (putStrLn msg)

data AppError
  = HttpError H.HttpException
  | AppApiError ApiError
  deriving (Show)

handleErrors :: Sem (Error H.HttpException : Error ApiError : Error AppError : r) a -> Sem r (Either AppError a)
handleErrors =
  runError @AppError
    . mapError AppApiError
    . mapError HttpError

data LogType = Info | Warning | LogError deriving stock (Eq, Show)

type LogMsg = (LogType, String)

type Logger = Log LogMsg

info :: Members '[Logger] r => String -> Sem r ()
info s = log (Info, s)

warn :: Members '[Logger] r => String -> Sem r ()
warn s = log (Warning, s)

err :: Members '[Logger] r => String -> Sem r ()
err s = log (LogError, s)

runLoggerOnRainbow :: (Members '[Embed IO] r) => InterpreterFor (Log LogMsg) r
runLoggerOnRainbow =
  interpret
    ( \(Log (logType, s)) -> case logType of
        Info -> embed $ putChunkLn (msg s)
        Warning -> embed $ putChunkLn $ fore yellow $ bold (msg s)
        LogError -> embed $ putChunkLn $ fore red $ bold (msg s)
    )
  where
    msg = chunk . T.pack

runLoggerAsOutput :: Sem (Log LogMsg : r) a -> Sem (Output LogMsg : r) a
runLoggerAsOutput =
  reinterpret
    ( \(Log logmsg) -> output logmsg
    )

newtype TransactionDate = TransactionDate Day deriving stock (Eq, Show, Generic) deriving anyclass (FromJSON, ToJSON)

instance CSV.ToField TransactionDate where
  toField (TransactionDate d) = BS.pack $ showGregorian d

data Transaction
  = Transaction
      { dated_on :: TransactionDate,
        description :: String,
        amount :: String
      }
  deriving stock (Eq, Generic, Show) deriving anyclass (FromJSON, CSV.ToRecord, ToJSON)

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving stock (Eq, Generic, Show) deriving anyclass (FromJSON, ToJSON)
