{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Config
  ( Config (..),
    updateConfig,
    bankAccounts,
    BankAccount (..),
    bankAccountId,
    lastImported,
    bankInstitution,
    BankInstitution (..),
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Types
import Prelude

data BankInstitution = Fa | Monzo
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey)

data BankAccount = BankAccount {_bankAccountId :: Int, _lastImported :: LastImported, _bankInstitution :: BankInstitution} deriving stock (Eq, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 1} ''BankAccount)

newtype Config
  = Config
      { _bankAccounts :: [BankAccount]
      }
  deriving stock (Eq, Generic, Show)

$(makeLenses ''BankAccount)

$(makeLenses ''Config)

$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 1} ''Config)

lastImportedById :: Int -> Traversal' Config LastImported
lastImportedById bID = bankAccounts . traversed . filtered (\ba -> _bankAccountId ba == bID) . lastImported

updateConfig :: Int -> LastImported -> Config -> Config
updateConfig bID newLastImported config = config & lastImportedById bID .~ newLastImported
