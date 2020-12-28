{-# LANGUAGE TemplateHaskell #-}

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
import Config
import Control.Lens
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
  GetNewTransactions :: BankAccount -> TransactionsManager m [Transaction]

$(makeSem ''TransactionsManager)

runTransactionsManager ::
  ( Members
      '[ TransactionsApiM,
         PersistLastImportedM,
         Logger
       ]
      r
  ) =>
  InterpreterFor TransactionsManager r
runTransactionsManager = interpret $ \case
  GetNewTransactions bankAccount -> do
    let (LastImported day) = bankAccount ^. lastImported
    info $ "Getting transactions after " ++ show day
    tx <- getTransactionsApi bankAccount day
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
