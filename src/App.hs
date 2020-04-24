{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module App
  ( app,
  )
where

import Control.Monad
import Polysemy
import Polysemy.Output
import Transaction
import Types
import Prelude hiding (log)

app :: (Members '[TransactionsManager, Output LastImported, Logger] r) => Sem r ()
app = do
  tx <- getTransactions
  info $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (warn "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  outputTransactions tx
