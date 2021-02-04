{-# LANGUAGE TemplateHaskell #-}

module Fa
  ( FaM (..),
    getFaTransactions,
    runFaM,
    FaTransaction (..),
    TransactionsEndpoint (..),
  )
where

import Config
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Text hiding (length)
import Data.Time
import GHC.Generics
import Logger
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Http
import Types
import Prelude hiding (log)

data FaTransaction = FaTransaction
  { dated_on :: TransactionDate,
    description :: Text,
    amount :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype TransactionsEndpoint = TransactionsEndpoint
  { bank_transactions :: [FaTransaction]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

faToTransaction :: FaTransaction -> Transaction
faToTransaction FaTransaction {..} =
  Transaction
    { dated_on = dated_on,
      description = description,
      amount = amount,
      comment = Nothing
    }

data FaM m a where
  GetFaTransactions :: BankAccount -> Day -> FaM m [Transaction]

$(makeSem ''FaM)

runFaRequest :: (Members '[ApiHttpM v] r) => Text -> Option 'Https -> Sem r (Either ApiError v)
runFaRequest endpoint = runApiRequest (https "api.freeagent.com" /: "v2" /: endpoint) GET NoReqBody

runFaM ::
  ( Members
      '[ ApiHttpM TransactionsEndpoint,
         Error ApiError,
         Logger
       ]
      r
  ) =>
  InterpreterFor FaM r
runFaM = interpret $ \case
  GetFaTransactions bankAccount fromDate -> do
    etx <-
      runFaRequest
        "bank_transactions"
        ( "bank_account" =: bankAccount ^. bankAccountId
            <> "from_date" =: fromDate
            <> "sort" =: ("dated_on" :: Text)
            <> "per_page" =: (100 :: Int)
        )
    case etx of
      Right endpoint -> do
        when (length tx == 100) (warn "WARNING: Number of transactions close to limit")
        return $ faToTransaction <$> tx
        where
          tx = bank_transactions endpoint
      Left e -> throw e
