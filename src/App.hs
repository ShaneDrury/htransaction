{-# LANGUAGE TemplateHaskell #-}

module App
  ( runApp,
    runWithDb,
    syncTransactions,
    AppM (..),
  )
where

import Config
import Db
import Logger
import Polysemy
import Polysemy.BankAccount
import Polysemy.Input
import Polysemy.Output
import Transaction
import Prelude hiding (log)

data AppM m a where
  SyncTransactions :: AppM m ()

$(makeSem ''AppM)

runWithDb :: (Members '[Embed IO, Input BankAccount, AppM] r) => FilePath -> Sem r a -> Sem r a
runWithDb fp = intercept @AppM $ \case
  SyncTransactions -> do
    institution <- getInstitution
    case institution of
      Fa -> syncTransactions
      Monzo -> do
        runMigrations fp
        syncTransactions

runApp :: (Members '[Input BankAccount, TransactionsManager, Output [Transaction], Logger] r) => InterpreterFor AppM r
runApp = interpret $ \case
  SyncTransactions -> do
    bankAccount <- input
    tx <- getNewTransactions bankAccount
    info $ "Number of transactions: " ++ show (length tx)
    output tx
