{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Control.Monad
import Data.Aeson
import Data.Time
import GHC.Generics
import Polysemy
import Polysemy.Trace

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Trace ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Trace msg -> embed $ when verbose (putStrLn msg)

newtype GeneralError e = GeneralError e deriving (Eq, Show)
