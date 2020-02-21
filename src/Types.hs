{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Time
import Data.Time.Clock
import GHC.Generics

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Message = Message String deriving (Eq, Show)
