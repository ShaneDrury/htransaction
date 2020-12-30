{-# LANGUAGE TemplateHaskell #-}

module App
  ( runApp,
    runWithDb,
    syncTransactions,
    AppM (..),
  )
where

import Config
import Database.Esqueleto
import qualified Database.Persist.Sqlite as P
import qualified Db as DB
import Logger
import Polysemy
import Polysemy.BankAccount
import Polysemy.Output
import Transaction
import Prelude hiding (log)

data AppM m a where
  SyncTransactions :: AppM m ()

$(makeSem ''AppM)

runWithDb :: (Members '[Embed IO, BankAccountsM, AppM] r) => Sem r a -> Sem r a
runWithDb = intercept @AppM $ \case
  SyncTransactions -> do
    institution <- getInstitution
    case institution of
      Fa -> syncTransactions
      Monzo -> do
        embed $ P.runSqlite ":memory:" (runMigration DB.migrateAll)
        syncTransactions

runApp :: (Members '[BankAccountsM, TransactionsManager, Output [Transaction], Logger] r) => InterpreterFor AppM r
runApp = interpret $ \case
  SyncTransactions -> do
    bankAccount <- getBankAccount
    tx <- getNewTransactions bankAccount
    info $ "Number of transactions: " ++ show (length tx)
    output tx
