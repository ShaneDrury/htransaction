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
  )
where

import qualified Control.Exception as E
import Control.Monad
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Text
import Data.Time.Clock
import qualified Db as DB
import GHC.Generics (Generic)
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Request
import Token
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
    notes :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

newtype MonzoTransactionsEndpoint = MonzoTransactionsEndpoint
  { transactions :: [MonzoTransaction]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

monzoRequest :: (MonadHttp m, FromJSON a) => Text -> ValidToken -> Option 'Https -> m (JsonResponse a)
monzoRequest endpoint (ValidToken tkn) options =
  req
    GET
    (https "api.monzo.com" /: endpoint)
    NoReqBody
    jsonResponse
    ( oAuth2Bearer tkn
        <> header
          "User-Agent"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
        <> options
    )

runMonzoM ::
  forall v r.
  ( Members
      '[ Embed IO,
         Error HttpException,
         ValidTokenM
       ]
      r,
    FromJSON v
  ) =>
  InterpreterFor (MonzoM v) r
runMonzoM = interpret $ \case
  GetMonzo endpoint options -> do
    tkn <- getValidToken
    result <- embed $
      E.try $ do
        r <- runReq defaultHttpConfig $ monzoRequest endpoint tkn options
        return (responseBody r :: v)
    case result of
      Right res -> return $ Right (res :: v)
      Left err' ->
        if isUnauthorized err'
          then return $ Left Unauthorized
          else throw @HttpException err'

-- TODO: possibly have to exclude pending items
-- include a "to" date in api request
-- can always have a bigger import window and let it dedupe

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

-- TODO:
-- now we have to deal with payment requests
-- initiated by not splitting a bill but an amount
-- these may have a note that we should use
-- they don't have a merchant seemingly
-- sometimes they have a blank note
-- so finally fall back to description

--priority
-- merchant name
-- non blank note
-- description
