{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    LogType (..),
  )
where

import Colog.Polysemy.Effect
import Control.Monad
import Data.Aeson
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

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Trace ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Trace msg -> embed $ when verbose (putStrLn msg)

newtype AppError
  = HttpError H.HttpException
  deriving (Show)

handleErrors :: Sem (Error H.HttpException : Error AppError : r) a -> Sem r (Either AppError a)
handleErrors =
  runError @AppError
    . mapError HttpError

data LogType = Info | Warning deriving stock (Eq, Show)

type LogMsg = (LogType, String)

type Logger = Log LogMsg

info :: Members '[Logger] r => String -> Sem r ()
info s = log (Info, s)

warn :: Members '[Logger] r => String -> Sem r ()
warn s = log (Warning, s)

runLoggerOnRainbow :: (Members '[Embed IO] r) => Sem (Log LogMsg ': r) a -> Sem r a
runLoggerOnRainbow =
  interpret
    ( \(Log (logType, s)) -> case logType of
        Info -> embed $ putChunkLn (msg s)
        Warning -> embed $ putChunkLn $ fore yellow $ bold (msg s)
    )
  where
    msg = chunk . T.pack

runLoggerAsOutput :: (Members '[Output LogMsg] r) => Sem (Log LogMsg ': r) a -> Sem r a
runLoggerAsOutput =
  interpret
    ( \(Log logmsg) -> output logmsg
    )
