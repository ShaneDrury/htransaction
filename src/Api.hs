{-# LANGUAGE TemplateHaskell #-}

module Api
  ( TransactionsApiM (..),
    getTransactionsApi,
    runTransactionsApiM,
  )
where

import Data.Time
import qualified Db as DB
import Fa
import Logger
import Monzo
import Polysemy
import Polysemy.BankAccount
import Polysemy.Error
import Polysemy.Input
import Types
import Prelude hiding (id, null)

data TransactionsApiM m a where
  GetTransactionsApi :: DB.BankAccount -> Day -> TransactionsApiM m [Transaction]

$(makeSem ''TransactionsApiM)

runTransactionsApiM :: (Members '[FaM, MonzoM, Error ApiError, Logger, Input DB.BankAccount] r) => InterpreterFor TransactionsApiM r
runTransactionsApiM = interpret $ \case
  GetTransactionsApi bankAccount fromDate -> do
    institution <- getInstitution
    case institution of
      Fa -> getFaTransactions bankAccount fromDate
      Monzo -> getMonzoTransactions bankAccount fromDate
