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
    getTransactions,
    runTransactionsManager,
    TransactionsEndpoint (..),
    NextTransactionsM (..),
    getNextTransactions,
    ShowTransactionsM,
    showTransactions,
    runNextTransactionsMOnLastImported,
    runShowTransactionsMEmpty,
    runShowTransactionsMOnList,
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

data ShowTransactionsM m a where
  ShowTransactions :: [Transaction] -> ShowTransactionsM m ()

$(makeSem ''ShowTransactionsM)

data TransactionsManager m a where
  GetTransactions :: Day -> TransactionsManager m [Transaction]

$(makeSem ''TransactionsManager)

data NextTransactionsM m a where
  GetNextTransactions :: NextTransactionsM m [Transaction]

$(makeSem ''NextTransactionsM)

runTransactionsManager ::
  ( Members
      '[ TransactionsApiM
       ]
      r
  ) =>
  Int ->
  InterpreterFor TransactionsManager r
runTransactionsManager bankAccountId = interpret $ \case
  GetTransactions fromDate -> getTransactionsApi bankAccountId fromDate

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
  InterpreterFor ShowTransactionsM r
runOutputOnCsv fp = interpret $ \case
  ShowTransactions tx -> do
    info $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

runShowTransactionsMEmpty :: InterpreterFor ShowTransactionsM r
runShowTransactionsMEmpty = interpret $ \case
  ShowTransactions _ -> return ()

runShowTransactionsMOnList :: Sem (ShowTransactionsM : r) a -> Sem r ([[Transaction]], a)
runShowTransactionsMOnList =
  runOutputList @[Transaction]
    . reinterpret
      ( \case
          ShowTransactions tx -> output tx
      )

runNextTransactionsMOnLastImported :: (Members '[TransactionsManager, GetLastImportedM, PersistLastImportedM, Logger] r) => InterpreterFor NextTransactionsM r
runNextTransactionsMOnLastImported = interpret $ \case
  GetNextTransactions -> do
    lastImported <- getLastImported
    info $ "Getting transactions after " ++ show lastImported
    tx <- getTransactions lastImported
    unless (null tx) (persistLastImported (latestTransaction tx))
    return tx
