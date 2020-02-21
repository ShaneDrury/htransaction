{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Time
import GHC.Generics
import Polysemy
import Polysemy.Output

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Message = Message String deriving (Eq, Show)

log' :: (Members '[Output Message] r) => String -> Sem r ()
log' msg = output $ Message msg
