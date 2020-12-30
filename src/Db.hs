{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Db (Transaction (..), migrateAll, TransactionId, relatedTransaction, DbM (..), runQuery, runDbMOnSqlite) where

import Data.Text
import Data.Time.Clock
import Database.Esqueleto hiding (get)
import qualified Database.Persist.Sqlite as P
import Database.Persist.TH
import Polysemy
import Prelude

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

data DbM m a where
  RunQuery :: SqlPersistM a -> DbM m a

$(makeSem ''DbM)

findByUuid :: Text -> SqlPersistM (Maybe (Entity Transaction))
findByUuid uuid = selectFirst [TransactionUuid P.==. uuid] []

relatedTransaction :: Transaction -> SqlPersistM (Maybe (Entity Transaction))
relatedTransaction tx = case transactionOriginalTransactionId tx of
  Just uuid -> findByUuid uuid
  Nothing -> return Nothing

-- TODO: wrap input/output transactions
-- and persist/modify, no need to add another sem yet

runDbMOnSqlite :: (Members '[Embed IO] r) => FilePath -> InterpreterFor DbM r
runDbMOnSqlite fp = interpret $ \case
  RunQuery qry -> embed $ P.runSqlite (pack fp) qry
