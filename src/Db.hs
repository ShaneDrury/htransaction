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
    DbM (..),
    runDbMOnSqlite,
    findByUuid,
    insertUnique,
    runMigrations,
    getAccount,
    BankAccountId,
    BankAccount (..),
    updateLastImported,
  )
where

import Config
import Data.Text
import Data.Time.Clock
import Database.Esqueleto hiding (get, insertUnique)
import qualified Database.Persist.Sqlite as P
import Database.Persist.TH
import Polysemy
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
|]

data DbM m a where
  FindByUuid :: Text -> DbM m (Maybe MonzoTransaction)
  InsertUnique :: MonzoTransaction -> DbM m (Maybe (Key MonzoTransaction))
  GetAccount :: Text -> DbM m (Maybe BankAccount)
  UpdateLastImported :: Text -> LastImported -> DbM m ()

$(makeSem ''DbM)

findByUuid_ :: Text -> SqlPersistM (Maybe (Entity MonzoTransaction))
findByUuid_ uuid = selectFirst [MonzoTransactionUuid P.==. uuid] []

accountByReference :: Text -> SqlPersistM (Maybe (Entity BankAccount))
accountByReference ref = selectFirst [BankAccountReference P.==. ref] []

updateLastImported_ :: Text -> LastImported -> SqlPersistM ()
updateLastImported_ baid lastImported = update $ \ba -> do
  set ba [BankAccountLastImported =. val lastImported]
  where_ (ba ^. BankAccountReference ==. val baid)

runDbMOnSqlite :: (Members '[Embed IO] r) => FilePath -> InterpreterFor DbM r
runDbMOnSqlite fp = interpret $ \case
  FindByUuid uuid -> embed $ do
    r <- P.runSqlite (pack fp) (findByUuid_ uuid)
    return $ entityVal <$> r
  InsertUnique tx -> embed $ P.runSqlite (pack fp) (P.insertUnique tx)
  GetAccount ref -> embed $
    P.runSqlite (pack fp) $ do
      account <- accountByReference ref
      return $ entityVal <$> account
  UpdateLastImported bankAccountId lastImported -> embed $ P.runSqlite (pack fp) $ updateLastImported_ bankAccountId lastImported

runMigrations :: (Members '[Embed IO] r) => FilePath -> Sem r ()
runMigrations fp = embed $ P.runSqlite (pack fp) (runMigration migrateAll)
