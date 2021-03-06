{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

module Db
  ( MonzoTransaction (..),
    migrateAll,
    MonzoTransactionId,
    BankAccountId,
    BankAccount (..),
    Client (..),
    TokenSet (..),
    TokenSetId,
    ClientId,
    EntityField
      ( MonzoTransactionUuid,
        BankAccountReference,
        BankAccountLastImported,
        TokenSetInstitution,
        ClientInstitution
      ),
  )
where

import Data.Text
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (get, insertUnique)
import Database.Persist.TH
import Types
import Prelude

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
MonzoTransaction
    description Text
    amount Int
    uuid Text
    UniqueMonzoTransactionUuid uuid
    dateTime UTCTime
    originalTransactionId Text Maybe
    merchantName Text Maybe
    note Text
    deriving Eq Show

BankAccount
    institution BankInstitution
    lastImported LastImported
    reference Text
    deriving Eq Show

TokenSet
    accessToken AccessToken
    refreshToken RefreshToken
    expiresAt UTCTime
    institution BankInstitution
    UniqueTokenSetInstitution institution
    deriving Eq Show

Client
    identifier Text
    secret Text
    institution BankInstitution
    UniqueClientInstitution institution
    deriving Eq Show
|]
