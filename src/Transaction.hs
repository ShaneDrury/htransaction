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
    runTransactionsApiM,
    runNextTransactionsMOnLastImported,
    TransactionsApiM (..),
    runShowTransactionsMEmpty,
    runShowTransactionsMOnList,
  )
where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import Data.Coerce
import qualified Data.Csv as CSV
import Data.List
import Data.Ord
import Data.Text hiding (null)
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Client as H
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.LastImported
import Types
import Control.Monad
import Prelude hiding (log)
import Fa

newtype TransactionDate = TransactionDate Day deriving stock (Eq, Show, Generic) deriving anyclass (FromJSON, ToJSON)

toDay :: TransactionDate -> Day
toDay = coerce

instance CSV.ToField TransactionDate where
  toField (TransactionDate d) = BS.pack $ showGregorian d

data Transaction
  = Transaction
      { dated_on :: TransactionDate,
        description :: String,
        amount :: String
      }
  deriving stock (Eq, Generic, Show) deriving anyclass (FromJSON, CSV.ToRecord, ToJSON)

data ShowTransactionsM m a where
  ShowTransactions :: [Transaction] -> ShowTransactionsM m ()

$(makeSem ''ShowTransactionsM)

data TransactionsManager m a where
  GetTransactions :: Day -> TransactionsManager m [Transaction]

$(makeSem ''TransactionsManager)

data TransactionsApiM m a where
  GetTransactionsApi :: Int -> Day -> TransactionsApiM m [Transaction]

$(makeSem ''TransactionsApiM)

data NextTransactionsM m a where
  GetNextTransactions :: NextTransactionsM m [Transaction]

$(makeSem ''NextTransactionsM)

runTransactionsManager ::
  ( Members
      '[ 
         TransactionsApiM
       ]
      r
  ) =>
  Int ->
  InterpreterFor TransactionsManager r
runTransactionsManager bankAccountId = interpret $ \case
  GetTransactions fromDate -> getTransactionsApi bankAccountId fromDate

runTransactionsApiM :: (Members '[FaM TransactionsEndpoint, Error ApiError] r) => InterpreterFor TransactionsApiM r
runTransactionsApiM = interpret $ \case
  GetTransactionsApi bankAccountId fromDate -> do
    etx <-
      getFa
        "bank_transactions"
        ( "bank_account" =: bankAccountId
            <> "from_date" =: fromDate
            <> "sort" =: ("dated_on" :: Text)
            <> "per_page" =: (100 :: Int)
        )
    case etx of
      Right r -> return $ bank_transactions r
      Left e -> throw e

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving stock (Eq, Generic, Show) deriving anyclass (FromJSON, ToJSON)

latestTransaction :: [Transaction] -> Day
latestTransaction tx = toDay . dated_on $ maximumBy (comparing (toDay . dated_on)) tx

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
  . reinterpret (
    \case
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
