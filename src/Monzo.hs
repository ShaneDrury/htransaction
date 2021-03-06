{-# LANGUAGE TemplateHaskell #-}

module Monzo
  ( MonzoM (..),
    getMonzoTransactions,
    runMonzoM,
    MonzoTransaction (..),
    MonzoTransactionsEndpoint (..),
    outputMonzoOnDb,
    MonzoMetadata (..),
    MonzoMerchant (..),
    toDbTransaction,
    excludeDeclinedTransactions,
    monzoAuthUrl,
    monzoAuthEndpoint,
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text hiding (filter)
import Data.Time
import qualified Db as DB
import GHC.Generics (Generic)
import Network.HTTP.Req
import Polysemy
import qualified Polysemy.Db as DB
import Polysemy.Error
import Polysemy.Http
import Polysemy.Output
import Types
import Prelude hiding (id, null)

data MonzoM m a where
  GetMonzoTransactions :: DB.BankAccount -> Day -> MonzoM m [Transaction]

$(makeSem ''MonzoM)

data MonzoMetadata = MonzoMetadata
  { original_transaction_id :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data MonzoMerchant = MonzoMerchant
  { name :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data MonzoTransaction = MonzoTransaction
  { amount :: Int,
    description :: Text,
    created :: UTCTime,
    id :: Text,
    metadata :: MonzoMetadata,
    merchant :: Maybe MonzoMerchant,
    notes :: Text,
    declined_reason :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

newtype MonzoTransactionsEndpoint = MonzoTransactionsEndpoint
  { transactions :: [MonzoTransaction]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

runMonzoRequest :: (Members '[ApiHttpM v] r) => Text -> Option 'Https -> Sem r (Either ApiError v)
runMonzoRequest endpoint = runApiRequest (https "api.monzo.com" /: endpoint) GET NoReqBody

relatedTransaction :: (Members '[DB.DbM] r) => DB.MonzoTransaction -> Sem r (Maybe DB.MonzoTransaction)
relatedTransaction tx = case DB.monzoTransactionOriginalTransactionId tx of
  Just uuid -> DB.findByUuid uuid
  Nothing -> return Nothing

useExistingTransaction :: MonzoTransaction -> DB.MonzoTransaction -> Transaction
useExistingTransaction MonzoTransaction {..} dbmonzo =
  Transaction
    { dated_on = TransactionDate $ utctDay created,
      description = descrip,
      amount = pack $ show (fromIntegral amount / 100.0 :: Double),
      comment = Just description
    }
  where
    descrip = case DB.monzoTransactionMerchantName dbmonzo of
      Just merchname -> merchname
      Nothing -> if null txNote then DB.monzoTransactionDescription dbmonzo else txNote
        where
          txNote = DB.monzoTransactionNote dbmonzo

monzoToTransaction :: MonzoTransaction -> Transaction
monzoToTransaction MonzoTransaction {..} =
  Transaction
    { dated_on = TransactionDate $ utctDay created,
      description = descrip,
      amount = pack $ show (fromIntegral amount / 100.0 :: Double),
      comment = Just description
    }
  where
    descrip = maybe (if null notes then description else notes) name merchant

createTransaction :: (Members '[DB.DbM] r) => MonzoTransaction -> Sem r Transaction
createTransaction tx = do
  result <- relatedTransaction (toDbTransaction tx)
  case result of
    Just other -> return $ useExistingTransaction tx other
    Nothing -> return $ monzoToTransaction tx

monzoAuthEndpoint :: Url 'Https
monzoAuthEndpoint = https "api.monzo.com" /: "oauth2" /: "token"

runMonzoM ::
  ( Members
      '[ ApiHttpM MonzoTransactionsEndpoint,
         Error ApiError,
         Output [MonzoTransaction],
         DB.DbM
       ]
      r
  ) =>
  InterpreterFor MonzoM r
runMonzoM = interpret $ \case
  GetMonzoTransactions bankAccount fromDate -> do
    etxs <-
      runMonzoRequest
        "transactions"
        ( "account_id" =: DB.bankAccountReference bankAccount
            <> "since" =: fromDate
            <> "expand[]" =: ("merchant" :: Text)
        )
    case etxs of
      Right endpoint -> do
        output @[MonzoTransaction] txs
        traverse createTransaction (excludeDeclinedTransactions txs)
        where
          txs = transactions endpoint
      Left e -> throw e

toDbTransaction :: MonzoTransaction -> DB.MonzoTransaction
toDbTransaction MonzoTransaction {..} =
  DB.MonzoTransaction
    { monzoTransactionDescription = description,
      monzoTransactionMerchantName = name <$> merchant,
      monzoTransactionAmount = amount,
      monzoTransactionUuid = id,
      monzoTransactionDateTime = created,
      monzoTransactionOriginalTransactionId = original_transaction_id metadata,
      monzoTransactionNote = notes
    }

outputMonzoOnDb :: (Members '[DB.DbM] r) => InterpreterFor (Output [MonzoTransaction]) r
outputMonzoOnDb = interpret $ \case
  Output txs -> traverse_ DB.insertUnique (toDbTransaction <$> txs)

maybeBool2Bool :: Maybe Bool -> Bool
maybeBool2Bool = fromMaybe False

excludeDeclinedTransactions :: [MonzoTransaction] -> [MonzoTransaction]
excludeDeclinedTransactions = filter (\tx -> not $ maybeBool2Bool (Data.Text.null <$> declined_reason tx))

monzoAuthUrl :: Text -> String -> Text
monzoAuthUrl clientId state =
  "https://auth.monzo.com/?client_id=" <> clientId <> "&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground&response_type=code&state=" <> pack state
