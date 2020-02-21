{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Transaction where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Csv as CSV
import Data.List
import Data.Ord
import Data.Text
import Data.Time
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import Token
import Types

data Transaction
  = Transaction
      { dated_on :: Day,
        description :: String,
        amount :: String
      }
  deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving (Eq, Generic, Show, FromJSON)

getTransactions :: Int -> Day -> ValidToken -> IO TransactionsEndpoint
getTransactions bankAccountId day (ValidToken token) = runReq defaultHttpConfig $ do
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
latestTransaction tx = LastImported $ dated_on $ maximumBy (comparing dated_on) tx

runInputTest :: (Members '[Embed IO] r) => Sem (Input [Transaction] ': r) a -> Sem r a
runInputTest = interpret $ \case
  Input -> return []

runOutputOnCsv :: (Members '[Embed IO, Trace] r) => FilePath -> Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    trace $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

runOutputOnStdout :: (Members '[Embed IO] r) => Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnStdout = interpret $ \case
  Output tx -> embed $ S.putStrLn (CSV.encode tx)

runInputOnNetwork :: (Members '[Embed IO, Input LastImported, Trace, Input ValidToken] r) => Int -> Sem (Input [Transaction] ': r) a -> Sem r a
runInputOnNetwork bankAccountId = interpret $ \case
  Input -> do
    (LastImported fromDate) <- input
    token <- input @ValidToken
    trace $ "Getting transactions from " ++ show bankAccountId ++ " after " ++ show fromDate
    embed $ bank_transactions <$> getTransactions bankAccountId fromDate token
