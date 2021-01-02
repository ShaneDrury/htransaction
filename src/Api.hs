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
import qualified Db as DB
import Fa
import GHC.Generics (Generic)
import Logger
import Monzo
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Types
import Prelude hiding (id, null)

data TransactionsApiM m a where
  GetTransactionsApi :: BankAccount -> Day -> TransactionsApiM m [Transaction]

$(makeSem ''TransactionsApiM)

newtype TransactionsEndpoint = TransactionsEndpoint
  { bank_transactions :: [Transaction]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

monzoToTransaction :: MonzoTransaction -> Transaction
monzoToTransaction MonzoTransaction {..} = Transaction {dated_on = TransactionDate $ utctDay created, description = descrip, amount = pack $ show (fromIntegral amount / 100.0 :: Double)}
  where
    descrip = maybe (if null notes then description else notes) name merchant

relatedTransaction :: (Members '[DB.DbM] r) => DB.MonzoTransaction -> Sem r (Maybe DB.MonzoTransaction)
relatedTransaction tx = case DB.monzoTransactionOriginalTransactionId tx of
  Just uuid -> DB.findByUuid uuid
  Nothing -> return Nothing

useExistingTransaction :: MonzoTransaction -> DB.MonzoTransaction -> Transaction
useExistingTransaction MonzoTransaction {..} dbmonzo = Transaction {dated_on = TransactionDate $ utctDay created, description = descrip, amount = pack $ show (fromIntegral amount / 100.0 :: Double)}
  where
    descrip = case DB.monzoTransactionMerchantName dbmonzo of
      Just merchname -> merchname
      Nothing -> if null txNote then DB.monzoTransactionDescription dbmonzo else txNote
        where
          txNote = DB.monzoTransactionNote dbmonzo

createTransaction :: (Members '[DB.DbM] r) => MonzoTransaction -> Sem r Transaction
createTransaction tx = do
  result <- relatedTransaction (toDbTransaction tx)
  case result of
    Just other -> return $ useExistingTransaction tx other
    Nothing -> return $ monzoToTransaction tx

runTransactionsApiM :: (Members '[FaM TransactionsEndpoint, MonzoM MonzoTransactionsEndpoint, Error ApiError, Logger, DB.DbM] r) => InterpreterFor TransactionsApiM r
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
              <> "expand[]" =: ("merchant" :: Text)
          )
      case etx of
        Right endpoint -> traverse createTransaction txs
          where
            txs = transactions endpoint
        Left e -> throw e
