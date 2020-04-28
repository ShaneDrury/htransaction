{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Transaction
  ( Transaction (..),
    TransactionDate (..),
    latestTransaction,
    runInputOnApi,
    runOutputOnCsv,
    retryOnUnauthorized,
    TransactionsManager (..),
    getTransactions,
    runTransactionsManager,
    outputTransactions,
    ApiManager (..),
    runApiManagerOnNetwork,
    TransactionsEndpoint (..),
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
import Data.Text
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Client as H
import Network.HTTP.Req
import qualified Network.HTTP.Types.Status as Status
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import Token
import Types
import Prelude hiding (log)

newtype TransactionDate = TransactionDate Day deriving (Eq, Show, Generic, FromJSON)

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
  deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

data TransactionsManager m a where
  GetTransactions :: TransactionsManager m [Transaction]
  OutputTransactions :: [Transaction] -> TransactionsManager m ()

makeSem ''TransactionsManager

-- TODO: Maybe eliminate Input [Transaction]

runTransactionsManager ::
  ( Members
      '[ Input (Either ApiError [Transaction]),
         Output [Transaction],
         Error ApiError
       ]
      r
  ) =>
  InterpreterFor TransactionsManager r
runTransactionsManager = interpret $ \case
  GetTransactions -> do
    etx <- input @(Either ApiError [Transaction])
    case etx of
      Right r -> return r
      Left e -> throw e
  OutputTransactions tx -> output tx

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving (Eq, Generic, Show, FromJSON)

getTransactionsNetwork :: (MonadHttp m, FromJSON a) => Int -> Day -> ValidToken -> m (JsonResponse a)
getTransactionsNetwork bankAccountId day (ValidToken token) = do
  req
    GET
    (https "api.freeagent.com" /: "v2" /: "bank_transactions")
    NoReqBody
    jsonResponse
    ( "bank_account" =: bankAccountId
        <> "from_date" =: day
        <> "sort" =: ("dated_on" :: Text)
        <> "per_page" =: (100 :: Int)
        <> oAuth2Bearer token
        <> header
          "User-Agent"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
    )

latestTransaction :: [Transaction] -> LastImported
latestTransaction tx = LastImported $ toDay . dated_on $ maximumBy (comparing (toDay . dated_on)) tx

runOutputOnCsv ::
  ( Members
      '[ Embed IO,
         Logger
       ]
      r
  ) =>
  FilePath ->
  InterpreterFor (Output [Transaction]) r
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    info $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

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

data ApiManager m a where
  GetApiTransactions :: Int -> Day -> ApiManager m (Either ApiError TransactionsEndpoint)

$(makeSem ''ApiManager)

runInputOnApi ::
  ( Members
      '[ LastImportedManager,
         ApiManager,
         Logger
       ]
      r
  ) =>
  Int ->
  InterpreterFor (Input (Either ApiError [Transaction])) r
runInputOnApi bankAccountId =
  interpret @(Input (Either ApiError [Transaction]))
    ( \case
        Input -> do
          (LastImported fromDate) <- getLastImported
          info $ "Getting transactions from " ++ show bankAccountId ++ " after " ++ show fromDate
          r <- getApiTransactions bankAccountId fromDate
          return $ bank_transactions <$> r
    )

runApiManagerOnNetwork ::
  ( Members
      '[ Embed IO,
         Error HttpException,
         Input ValidToken
       ]
      r
  ) =>
  InterpreterFor ApiManager r
runApiManagerOnNetwork = interpret $ \case
  GetApiTransactions bankAccountId fromDate -> do
    token <- input @ValidToken
    result <- embed $ E.try $ do
      r <- runReq defaultHttpConfig $ getTransactionsNetwork bankAccountId fromDate token
      return (responseBody $ r :: TransactionsEndpoint)
    case result of
      Right r -> return $ Right r
      Left err' ->
        if isUnauthorized err'
          then return $ Left Unauthorized
          else throw @HttpException err'

-- TODO: At this point, check result is unauthed
-- throw specific Unauthed error, or Either it.
-- possibly can get rid of apperror, as it would only cover H.HttpException

retryOnUnauthorized ::
  Members
    '[ Logger,
       Input (Either ApiError [Transaction]),
       Input (Tagged Refresh TokenEndpoint),
       Error HttpException
     ]
    r =>
  Sem r a ->
  Sem r a
retryOnUnauthorized =
  intercept @(Input (Either ApiError [Transaction]))
    ( \case
        Input -> do
          r <- input @(Either ApiError [Transaction])
          case r of
            Left Unauthorized -> do
              err "Unauthorized"
              input @(Tagged Refresh TokenEndpoint)
              input @(Either ApiError [Transaction])
            s -> return s
    )
