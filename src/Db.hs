{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

module Db (MonzoTransaction (..), migrateAll, MonzoTransactionId, DbM (..), runDbMOnSqlite, findByUuid, insertUnique, runMigrations) where

import Data.Text
import Data.Time.Clock
import Database.Esqueleto hiding (get, insertUnique)
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
    UniqueMonzoTransactionUuid uuid
    dateTime UTCTime
    originalTransactionId Text Maybe
    merchantName Text Maybe
    note Text
    deriving Eq Show
|]

data DbM m a where
  FindByUuid :: Text -> DbM m (Maybe MonzoTransaction)
  InsertUnique :: MonzoTransaction -> DbM m (Maybe (Key MonzoTransaction))

$(makeSem ''DbM)

findByUuid_ :: Text -> SqlPersistM (Maybe (Entity MonzoTransaction))
findByUuid_ uuid = selectFirst [MonzoTransactionUuid P.==. uuid] []

runDbMOnSqlite :: (Members '[Embed IO] r) => FilePath -> InterpreterFor DbM r
runDbMOnSqlite fp = interpret $ \case
  FindByUuid uuid -> embed $ do
    r <- P.runSqlite sfp (findByUuid_ uuid)
    return $ entityVal <$> r
  InsertUnique tx -> embed $ P.runSqlite sfp (P.insertUnique tx)
  where
    sfp = pack fp

runMigrations :: (Members '[Embed IO] r) => FilePath -> Sem r ()
runMigrations fp = embed $ P.runSqlite (pack fp) (runMigration migrateAll)
