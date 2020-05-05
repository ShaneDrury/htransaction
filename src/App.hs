{-# LANGUAGE DataKinds #-}

module App
  ( app,
  )
where

import Control.Monad
import Polysemy
import Transaction
import Types
import Prelude hiding (log)

app :: (Members '[NextTransactionsM, ShowTransactionsM, Logger] r) => Sem r ()
app = do
  tx <- getNextTransactions
  info $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (warn "WARNING: Number of transactions close to limit")
  showTransactions tx
