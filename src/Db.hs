{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Db (MonzoTransaction (..), migrateAll, MonzoTransactionId, relatedTransaction, DbM (..), runQuery, runDbMOnSqlite) where

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
MonzoTransaction
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

findByUuid :: Text -> SqlPersistM (Maybe (Entity MonzoTransaction))
findByUuid uuid = selectFirst [MonzoTransactionUuid P.==. uuid] []

relatedTransaction :: MonzoTransaction -> SqlPersistM (Maybe (Entity MonzoTransaction))
relatedTransaction tx = case monzoTransactionOriginalTransactionId tx of
  Just uuid -> findByUuid uuid
  Nothing -> return Nothing

-- TODO: wrap input/output transactions
-- and persist/modify, no need to add another sem yet

runDbMOnSqlite :: (Members '[Embed IO] r) => FilePath -> InterpreterFor DbM r
runDbMOnSqlite fp = interpret $ \case
  RunQuery qry -> embed $ P.runSqlite (pack fp) qry
