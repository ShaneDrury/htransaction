{-# LANGUAGE TemplateHaskell #-}

module Config
  ( BankInstitution (..),
  )
where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics
import Prelude

data BankInstitution = Fa | Monzo
  deriving stock (Eq, Show, Generic, Ord, Read)
  deriving anyclass (FromJSON, ToJSON)

derivePersistField "BankInstitution"

instance ToJSONKey BankInstitution where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey BankInstitution where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
