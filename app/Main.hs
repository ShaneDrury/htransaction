{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Main where

import Lib
import Data.Time
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy.Char8 as S
import Polysemy
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import Data.Either
import qualified Data.Csv as CSV
import System.FilePath
import Data.Semigroup ((<>))
import Options.Generic

data Transaction = Transaction {
  --url :: String,
  dated_on :: Day,
  description :: String,
  amount :: String
  --full_description :: String
                                 } deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

data Args = Args {
  outfile :: FilePath
  --importFrom :: Day
                       } deriving (Eq, Generic, Show)

instance ParseRecord Args where

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

data TransactionsEndpoint = TransactionsEndpoint {
  bank_transactions :: [Transaction]
                                                 } deriving (Eq, Generic, Show, FromJSON)

runInputOnFile :: (Members '[Embed IO] r) => FilePath -> Sem (Input (Either String [Transaction]) ': r) a -> Sem r a
runInputOnFile fp = interpret $ \case
  Input -> do
    er <- embed $ eitherDecodeFileStrict fp
    return $ bank_transactions <$> er

runOutputOnCsv :: (Members '[Embed IO] r) => FilePath -> Sem (Output S.ByteString ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output csv -> embed $ S.writeFile fp csv

runOutputOnStdout :: (Members '[Embed IO] r) => Sem (Output S.ByteString ': r) a -> Sem r a
runOutputOnStdout = interpret $ \case
  Output csv -> embed $ S.putStrLn csv

runInputOnStdin :: (Members '[Embed IO] r) => Sem (Input (Either String [Transaction]) ': r) a -> Sem r a
runInputOnStdin = interpret $ \case
  Input -> do
    json <- embed BS.getContents
    return $ bank_transactions <$> eitherDecodeStrict json

runapp Args{..} sem = runM . runOutputOnCsv outfile . runInputOnStdin $ sem

-- runapp Args{..} sem = runM . runOutputOnStdout . runInputOnStdin $ sem

app :: (Members '[Embed IO, Input (Either String [Transaction]), Output S.ByteString] r) => Sem r ()
app = do
  transactions <- input
  case transactions of
    Left e -> embed $ print e
    Right tx -> output (CSV.encode tx)

main :: IO ()
main = do
  options <- getRecord "Test program"
  runapp options app

-- TODO: Inspect last transaction date and create last_date from that, otherwise we'll lose transactions

