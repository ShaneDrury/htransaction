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
  )
where

import qualified Control.Exception as E
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Time.Clock
import Database.Esqueleto
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
  { original_transaction_id :: Maybe String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data MonzoTransaction = MonzoTransaction
  { amount :: Int,
    description :: String,
    created :: UTCTime,
    id :: String,
    metadata :: MonzoMetadata
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

-- idea - intercept this and log out more info - helps in reconciling
-- or possibly inspect info and deduce it's splitting a bill
-- in which case use the original merchant as the description?
-- could also spit out an extra comment

-- possibly have to exclude pending items
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
    { monzoTransactionDescription = pack description,
      monzoTransactionAmount = amount,
      monzoTransactionUuid = pack id,
      monzoTransactionDateTime = created,
      monzoTransactionOriginalTransactionId = pack <$> original_transaction_id metadata
    }

outputMonzoOnDb :: (Members '[DB.DbM] r) => InterpreterFor (Output [MonzoTransaction]) r
outputMonzoOnDb = interpret $ \case
  Output txs -> DB.runQuery (insertMany_ (toDbTransaction <$> txs))

-- TODO: Insert only if don't exist
-- can constrain unique id

-- TODO: Instead of storing ids for existing
-- could store id of matching transaction
-- maybe a data consistency issue?
-- also depends on syncing that one first
