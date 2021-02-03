{-# LANGUAGE TemplateHaskell #-}

module Monzo
  ( MonzoM (..),
    getMonzo,
    runMonzoM,
    MonzoTransaction (..),
    MonzoTransactionsEndpoint (..),
    outputMonzoTransactions,
    outputMonzoOnDb,
    MonzoMetadata (..),
    MonzoMerchant (..),
    toDbTransaction,
    excludeDeclinedTransactions,
  )
where

import Control.Monad
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text hiding (filter)
import Data.Time.Clock
import qualified Db as DB
import GHC.Generics (Generic)
import Network.HTTP.Req
import Polysemy
import Polysemy.Http
import Polysemy.Output
import Types hiding (amount, description)
import Prelude hiding (id)

data MonzoM v m a where
  GetMonzo :: Text -> Option 'Https -> MonzoM v m (Either ApiError v)

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

runMonzoM ::
  forall v r.
  ( Members
      '[ ApiHttpM v
       ]
      r
  ) =>
  InterpreterFor (MonzoM v) r
runMonzoM = interpret $ \case
  GetMonzo endpoint options -> runApiRequest (https "api.monzo.com" /: endpoint) GET NoReqBody options

outputMonzoTransactions :: (Members '[MonzoM MonzoTransactionsEndpoint, Output [MonzoTransaction]] r) => Sem r a -> Sem r a
outputMonzoTransactions = intercept $ \case
  GetMonzo endpoint options -> do
    etxs <- getMonzo endpoint options
    case etxs of
      Right txs -> output $ transactions txs
      _ -> return ()
    return etxs

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
