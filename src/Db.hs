{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Db (Transaction (..), migrateAll, TransactionId, relatedTransaction) where

import Data.Time.Clock
import qualified Database.Persist.Sqlite as P
import Database.Persist.TH
import Prelude
import Database.Esqueleto
import Data.Text

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Transaction
    description Text
    amount Int
    uuid Text
    dateTime UTCTime
    originalTransactionId Text Maybe
    deriving Eq Show
|]

findByUuid :: Text -> SqlPersistM (Maybe (Entity Transaction))
findByUuid uuid = selectFirst [TransactionUuid P.==. uuid] []

relatedTransaction :: Transaction -> SqlPersistM (Maybe (Entity Transaction))
relatedTransaction tx = case transactionOriginalTransactionId tx of
  Just uuid -> findByUuid uuid
  Nothing -> return Nothing
