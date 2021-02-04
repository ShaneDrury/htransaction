{-# LANGUAGE TemplateHaskell #-}

module Api
  ( TransactionsApiM (..),
    getTransactionsApi,
    runTransactionsApiM,
  )
where

import Config
import Control.Lens
import Data.Time
import qualified Db as DB
import Fa
import Logger
import Monzo
import Polysemy
import Polysemy.Error
import Types
import Prelude hiding (id, null)

data TransactionsApiM m a where
  GetTransactionsApi :: BankAccount -> Day -> TransactionsApiM m [Transaction]

$(makeSem ''TransactionsApiM)

runTransactionsApiM :: (Members '[FaM, MonzoM, Error ApiError, Logger, DB.DbM] r) => InterpreterFor TransactionsApiM r
runTransactionsApiM = interpret $ \case
  GetTransactionsApi bankAccount fromDate -> case bankAccount ^. bankInstitution of
    Fa -> getFaTransactions bankAccount fromDate
    Monzo -> getMonzoTransactions bankAccount fromDate
