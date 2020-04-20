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
import Polysemy.Trace
import Transaction
import Types
import Prelude

app :: (Members '[TransactionsManager, Output LastImported, Trace] r) => Sem r ()
app = do
  tx <- getTransactions
  trace $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (trace "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  outputTransactions tx
