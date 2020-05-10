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
    retryOnUnauthorized,
    TransactionsManager (..),
    getTransactions,
    runTransactionsManager,
    TransactionsEndpoint (..),
    NextTransactionsM (..),
    getNextTransactions,
    ShowTransactionsM,
    showTransactions,
    runFaM,
    runTransactionsApiM,
    runNextTransactionsMOnLastImported,
    TransactionsApiM (..),
    FaM (..),
    runShowTransactionsMEmpty,
    runShowTransactionsMOnList,
  )
where

import qualified Control.Exception as E
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import Data.Coerce
import qualified Data.Csv as CSV
import Data.List
import Data.Ord
import Data.Tagged
import Data.Text hiding (null)
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Client as H
import Network.HTTP.Req
import qualified Network.HTTP.Types.Status as Status
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.LastImported
import Token
import Types
import Control.Monad
import Prelude hiding (log)

newtype TransactionDate = TransactionDate Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

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
  deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord, ToJSON)

data ShowTransactionsM m a where
  ShowTransactions :: [Transaction] -> ShowTransactionsM m ()

$(makeSem ''ShowTransactionsM)

data TransactionsManager m a where
  GetTransactions :: Day -> TransactionsManager m [Transaction]

makeSem ''TransactionsManager

data TransactionsApiM m a where
  GetTransactionsApi :: Int -> Day -> TransactionsApiM m [Transaction]

makeSem ''TransactionsApiM

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

data FaM v m a where
  GetFa :: Text -> Option 'Https -> FaM v m (Either ApiError v)

$(makeSem ''FaM)

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
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

faRequest :: (MonadHttp m, FromJSON a) => Text -> ValidToken -> Option 'Https -> m (JsonResponse a)
faRequest endpoint (ValidToken token) options =
  req
    GET
    (https "api.freeagent.com" /: "v2" /: endpoint)
    NoReqBody
    jsonResponse
    ( oAuth2Bearer token
        <> header
          "User-Agent"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
        <> options
    )

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

$(makePrisms ''HttpException) -- req

$(makePrisms ''H.HttpException)

$(makePrisms ''H.HttpExceptionContent)

responseP :: Traversal' HttpException (H.Response ())
responseP = _VanillaHttpException . _HttpExceptionRequest . _2 . _StatusCodeException . _1

statusP :: HttpException -> Maybe Status.Status
statusP e = H.responseStatus <$> e ^? responseP

isUnauthorized :: HttpException -> Bool
isUnauthorized e = case statusP e of
  Just status -> status == Status.unauthorized401
  Nothing -> False

runFaM ::
  forall v r.
  ( Members
      '[ Embed IO,
         Error HttpException,
         ValidTokenM
       ]
      r,
    FromJSON v
  ) =>
  InterpreterFor (FaM v) r
runFaM = interpret $ \case
  GetFa endpoint options -> do
    token <- getValidToken
    result <- embed $ E.try $ do
      r <- runReq defaultHttpConfig $ faRequest endpoint token options
      return (responseBody r :: v)
    case result of
      Right res -> return $ Right (res :: v)
      Left err' ->
        if isUnauthorized err'
          then return $ Left Unauthorized
          else throw @HttpException err'

-- TODO: At this point, check result is unauthed
-- throw specific Unauthed error, or Either it.
-- possibly can get rid of apperror, as it would only cover H.HttpException

retryOnUnauthorized ::
  forall r a.
  (Members
    '[ Logger,
       FaM TransactionsEndpoint,
       ValidTokenM
     ]
    r
    ) =>
  Sem r a ->
  Sem r a
retryOnUnauthorized =
  intercept @(FaM TransactionsEndpoint)
    ( \case
        GetFa endpoint option -> do
          r <- getFa @TransactionsEndpoint endpoint option
          case r of
            Left Unauthorized -> do
              err "Unauthorized"
              invalidateTokens
              getFa endpoint option
            s -> return s
    )

data NextTransactionsM m a where
  GetNextTransactions :: NextTransactionsM m [Transaction]

$(makeSem ''NextTransactionsM)

runNextTransactionsMOnLastImported :: (Members '[TransactionsManager, GetLastImportedM, PersistLastImportedM, Logger] r) => InterpreterFor NextTransactionsM r
runNextTransactionsMOnLastImported = interpret $ \case
  GetNextTransactions -> do
    lastImported <- getLastImported
    info $ "Getting transactions after " ++ show lastImported
    tx <- getTransactions lastImported
    unless (null tx) (persistLastImported (latestTransaction tx))
    return tx
