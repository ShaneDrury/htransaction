{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Main where

import Lib
import Data.Time
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import Polysemy
import Polysemy.Embed
import Data.Either
import qualified Data.Csv as CSV
import System.FilePath

data Transaction = Transaction {
  --url :: String,
  dated_on :: Day,
  description :: String,
  amount :: String
  --full_description :: String
                                 } deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

data TransactionsEndpoint = TransactionsEndpoint {
  bank_transactions :: [Transaction]
                                                 } deriving (Eq, Generic, Show, FromJSON)

data TransactionsProvider m a where
  GetTransactions :: TransactionsProvider m [Transaction]

makeSem ''TransactionsProvider

runTransactionsProviderOnFile :: (Members '[Embed IO] r) => FilePath -> Sem (TransactionsProvider ': r) a -> Sem r a
runTransactionsProviderOnFile fp = interpret $ \case
  GetTransactions -> embed $ bank_transactions <$> fromRight undefined <$> eitherDecodeFileStrict fp

runapp sem = runM . runTransactionsProviderOnFile
  ("/Users" </> "shanedrury" </> "repos" </> "htransaction" </> "sample.json") $ sem

app :: (Members '[Embed IO, TransactionsProvider] r) => Sem r ()
app = do
  transactions <- getTransactions
  let csv = CSV.encode transactions
  embed $ S.putStrLn csv

main :: IO ()
main = do
  runapp app
