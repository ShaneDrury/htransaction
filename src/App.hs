{-# LANGUAGE DataKinds #-}

module App
  ( app,
  )
where

import Control.Monad
import Logger
import Polysemy
import Transaction
import Prelude hiding (log)

app :: (Members '[TransactionsManager, ShowTransactionsM, Logger] r) => Sem r ()
app = do
  tx <- getNewTransactions
  info $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (warn "WARNING: Number of transactions close to limit")
  showTransactions tx
