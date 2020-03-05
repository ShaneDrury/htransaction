{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Types
  ( runOutputOnLog,
    LastImported (..),
    handleErrors,
    AppError (..),
  )
where

import Control.Monad
import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import Prelude

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Trace ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Trace msg -> embed $ when verbose (putStrLn msg)

data AppError
  = HttpError H.HttpException
  | UnauthorizedError
  deriving (Show)

handleErrors :: Sem (Error H.HttpException : Error AppError : r) a -> Sem r (Either AppError a)
handleErrors =
  runError @AppError
    . mapError HttpError
