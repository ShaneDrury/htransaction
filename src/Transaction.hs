{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Transaction
  ( Transaction,
    latestTransaction,
    runInputOnNetwork,
    runOutputOnCsv,
    retryOnUnauthorized,
    TransactionsManager,
    getTransactions,
    runTransactionsManager,
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
import Data.Text
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Client as H
import Network.HTTP.Req
import qualified Network.HTTP.Types.Status as Status
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import Token
import Types
import Prelude

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

makeSem ''TransactionsManager

runTransactionsManager :: (Members '[Input [Transaction]] r) => Sem (TransactionsManager : r) a -> Sem r a
runTransactionsManager = interpret $ \case
  GetTransactions -> input @[Transaction]

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving (Eq, Generic, Show, FromJSON)

getTransactionsNetwork :: Int -> Day -> ValidToken -> IO TransactionsEndpoint
getTransactionsNetwork bankAccountId day (ValidToken token) = runReq defaultHttpConfig $ do
  r <-
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
          <> header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
  return (responseBody r :: TransactionsEndpoint)

latestTransaction :: [Transaction] -> LastImported
latestTransaction tx = LastImported $ toDay . dated_on $ maximumBy (comparing (toDay . dated_on)) tx

runOutputOnCsv :: (Members '[Embed IO, Trace] r) => FilePath -> Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    trace $ "Writing to " ++ fp
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

runInputOnNetwork :: (Members '[Embed IO, Input LastImported, Trace, Input ValidToken, Error HttpException] r) => Int -> Sem (Input [Transaction] : r) a -> Sem r a
runInputOnNetwork bankAccountId =
  interpret @(Input [Transaction])
    ( \case
        Input -> do
          (LastImported fromDate) <- input
          token <- input @ValidToken
          trace $ "Getting transactions from " ++ show bankAccountId ++ " after " ++ show fromDate
          result <- embed $ E.try (getTransactionsNetwork bankAccountId fromDate token)
          case result of
            Right r -> return $ bank_transactions r
            Left err -> throw @HttpException err
    )

retryOnUnauthorized :: Members '[Trace, Input [Transaction], Output InvalidToken, Error HttpException] r => Sem r a -> Sem r a
retryOnUnauthorized =
  intercept @(Input [Transaction])
    ( \case
        Input ->
          catch @HttpException
            input
            ( \e ->
                if isUnauthorized e
                  then
                    ( do
                        trace "Unauthorized"
                        output InvalidToken
                        input
                    )
                  else throw e
            )
    )
