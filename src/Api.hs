{-# LANGUAGE TemplateHaskell #-}

module Api
  ( TransactionsApiM (..),
    getTransactionsApi,
    runTransactionsApiM,
    TransactionsEndpoint (..),
  )
where

import Config
import Control.Lens
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text hiding (length)
import Data.Time
import Fa
import GHC.Generics (Generic)
import Logger
import Monzo
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Types
import Prelude hiding (id)

data TransactionsApiM m a where
  GetTransactionsApi :: BankAccount -> Day -> TransactionsApiM m [Transaction]

$(makeSem ''TransactionsApiM)

newtype TransactionsEndpoint = TransactionsEndpoint
  { bank_transactions :: [Transaction]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

monzoToTransaction :: MonzoTransaction -> Transaction
monzoToTransaction MonzoTransaction {..} = Transaction {dated_on = TransactionDate $ utctDay created, description = description, amount = show (fromIntegral amount / 100.0 :: Double)}

runTransactionsApiM :: (Members '[FaM TransactionsEndpoint, MonzoM MonzoTransactionsEndpoint, Error ApiError, Logger] r) => InterpreterFor TransactionsApiM r
runTransactionsApiM = interpret $ \case
  GetTransactionsApi bankAccount fromDate -> case bankAccount ^. bankInstitution of
    Fa -> do
      etx <-
        getFa
          "bank_transactions"
          ( "bank_account" =: bankAccount ^. bankAccountId
              <> "from_date" =: fromDate
              <> "sort" =: ("dated_on" :: Text)
              <> "per_page" =: (100 :: Int)
          )
      case etx of
        Right endpoint -> do
          when (length tx == 100) (warn "WARNING: Number of transactions close to limit")
          return tx
          where
            tx = bank_transactions endpoint
        Left e -> throw e
    Monzo -> do
      etx <-
        getMonzo @MonzoTransactionsEndpoint
          "transactions"
          ( "account_id" =: bankAccount ^. bankAccountId
              <> "since" =: fromDate
          )
      case etx of
        Right endpoint -> return $ monzoToTransaction <$> txs
          where
            txs = transactions endpoint
        Left e -> throw e
