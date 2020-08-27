{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Transaction
  ( Transaction (..),
    TransactionDate (..),
    latestTransaction,
    runOutputOnCsv,
    TransactionsManager (..),
    runTransactionsManager,
    getNewTransactions,
    runShowTransactionsMEmpty,
  )
where

import Api
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as S
import Data.Coerce
import qualified Data.Csv as CSV
import Data.List
import Data.Ord
import Data.Time
import Logger
import Polysemy
import Polysemy.LastImported
import Polysemy.Output
import Types
import Prelude hiding (log)

data TransactionsManager m a where
  GetNewTransactions :: TransactionsManager m [Transaction]

$(makeSem ''TransactionsManager)

runTransactionsManager ::
  ( Members
      '[ TransactionsApiM,
         GetLastImportedM,
         PersistLastImportedM,
         Logger
       ]
      r
  ) =>
  Int ->
  InterpreterFor TransactionsManager r
runTransactionsManager bankAccountId = interpret $ \case
  GetNewTransactions -> do
    lastImported <- getLastImported
    info $ "Getting transactions after " ++ show lastImported
    tx <- getTransactionsApi bankAccountId lastImported
    unless (null tx) (persistLastImported (latestTransaction tx))
    return tx

toDay :: TransactionDate -> Day
toDay = coerce

latestTransaction :: [Transaction] -> Day
latestTransaction = toDay . dated_on . maximumBy (comparing dated_on)

runOutputOnCsv ::
  ( Members
      '[ Embed IO,
         Logger
       ]
      r
  ) =>
  FilePath ->
  InterpreterFor (Output [Transaction]) r
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    info $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

runShowTransactionsMEmpty :: InterpreterFor (Output [Transaction]) r
runShowTransactionsMEmpty = interpret $ \case
  Output _ -> return ()
