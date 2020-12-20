{-# LANGUAGE DataKinds #-}

module App
  ( app,
  )
where

import Control.Monad
import Logger
import Polysemy
import Polysemy.Config
import Polysemy.Output
import Transaction
import Prelude hiding (log)

app :: (Members '[BankAccountsM, TransactionsManager, Output [Transaction], Logger] r) => Sem r ()
app = do
  bankAccount <- getBankAccount
  tx <- getNewTransactions bankAccount
  info $ "Number of transactions: " ++ show (length tx)
  output tx
