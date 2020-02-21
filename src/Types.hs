{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Control.Monad
import Data.Aeson
import qualified Data.Map as Map
import Data.Time
import GHC.Generics
import Polysemy
import Polysemy.Input
import Polysemy.Output

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Message = Message String deriving (Eq, Show)

log' :: (Members '[Output Message] r) => String -> Sem r ()
log' msg = output $ Message msg

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Output Message ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Output (Message msg) -> embed $ when verbose (putStrLn msg)
