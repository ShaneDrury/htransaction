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
import qualified Data.ByteString.Lazy as BS
import Polysemy
import Polysemy.Embed
import Data.Either

data Transaction = Transaction {
  url :: String,
  amount :: String,
  dated_on :: Day,
  description :: String
  --full_description :: String
                                 } deriving (Eq, Generic, Show, FromJSON)

data TransactionsEndpoint = TransactionsEndpoint {
  bank_transactions :: [Transaction]
                                                 } deriving (Eq, Generic, Show, FromJSON)

data TransactionsProvider m a where
  GetTransactions :: TransactionsProvider m [Transaction]

makeSem ''TransactionsProvider

runTransactionsProviderOnFile :: (Members '[Embed IO] r) => FilePath -> Sem (TransactionsProvider ': r) a -> Sem r a
runTransactionsProviderOnFile fp = interpret $ \case
  GetTransactions -> embed $ bank_transactions <$> fromRight undefined <$> eitherDecodeFileStrict fp

runapp sem = runM . runTransactionsProviderOnFile "sample.json" $ sem

app :: (Members '[Embed IO, TransactionsProvider] r) => Sem r ()
app = do
  transactions <- getTransactions
  embed $ print transactions

main :: IO ()
main = do
  runapp app
