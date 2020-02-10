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
import Data.List
import Data.Ord
import Control.Monad

data Transaction = Transaction {
  dated_on :: Day,
  description :: String,
  amount :: String
                                 } deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

data Args = Args {
  outfile :: FilePath,
  configFile :: FilePath,
  verbose :: Bool
                       } deriving (Eq, Generic, Show)

data LastImported = LastImported Day deriving (Eq, Show)

data Message = Message String deriving (Eq, Show)

data Config = Config {
  lastImportedDate :: LastImported
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

runOutputOnCsv :: (Members '[Embed IO, Output Message] r) => FilePath -> Sem (Output S.ByteString ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output csv -> do
    output $ Message $ "Writing to " ++ fp
    embed $ S.writeFile fp csv

runOutputOnStdout :: (Members '[Embed IO] r) => Sem (Output S.ByteString ': r) a -> Sem r a
runOutputOnStdout = interpret $ \case
  Output csv -> embed $ S.putStrLn csv

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[Embed IO, Output Message] r) => FilePath -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp = interpret $ \case
  Output (LastImported day) -> do
    output $ Message $ "Writing last imported day of " ++ (show day) ++ " to " ++ fp
    embed $ writeFile fp (show day)

runInputOnStdin :: (Members '[Embed IO] r) => Sem (Input (Either String [Transaction]) ': r) a -> Sem r a
runInputOnStdin = interpret $ \case
  Input -> do
    json <- embed BS.getContents
    return $ bank_transactions <$> eitherDecodeStrict json

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Output Message ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Output (Message msg) -> embed $ when verbose (putStrLn msg)

runapp Args{..} sem = runM . runOutputOnLog verbose . runOutputLastImportedOnFile configFile . runOutputOnCsv outfile . runInputOnStdin $ sem

latestTransaction :: [Transaction] -> LastImported
latestTransaction tx = LastImported $ dated_on $ maximumBy (comparing dated_on) tx

app :: (Members '[Embed IO, Input (Either String [Transaction]), Output S.ByteString, Output LastImported, Output Message] r) => Sem r ()
app = do
  transactions <- input
  case transactions of
    Left e -> embed $ print e
    Right tx -> do
      output $ Message $ "Number of transactions: " ++ show (length tx)
      when (length tx >= 1) (output (latestTransaction tx))
      output (CSV.encode tx)

main :: IO ()
main = do
  options <- getRecord "Test program"
  runapp options app
