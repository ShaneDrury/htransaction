{-# LANGUAGE TemplateHaskell #-}

module App
  ( runApp,
    syncTransactions,
    AppM (..),
  )
where

import Db
import Logger
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Transaction
import Prelude hiding (log)

data AppM m a where
  SyncTransactions :: AppM m ()

$(makeSem ''AppM)

runApp :: (Members '[Input BankAccount, TransactionsManager, Output [Transaction], Logger] r) => InterpreterFor AppM r
runApp = interpret $ \case
  SyncTransactions -> do
    bankAccount <- input
    tx <- getNewTransactions bankAccount
    info $ "Number of transactions: " ++ show (length tx)
    output tx
