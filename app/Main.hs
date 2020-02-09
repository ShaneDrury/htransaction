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
import Polysemy.Input
import Polysemy.Output
import Data.Either
import qualified Data.Csv as CSV
import System.FilePath
import Options.Applicative
import Data.Semigroup ((<>))

data Transaction = Transaction {
  --url :: String,
  dated_on :: Day,
  description :: String,
  amount :: String
  --full_description :: String
                                 } deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

data Args = Args {
  outfile :: FilePath
                       }

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

data TransactionsEndpoint = TransactionsEndpoint {
  bank_transactions :: [Transaction]
                                                 } deriving (Eq, Generic, Show, FromJSON)

runTransactionsProviderOnFile :: (Members '[Embed IO] r) => FilePath -> Sem (Input [Transaction] ': r) a -> Sem r a
runTransactionsProviderOnFile fp = interpret $ \case
  Input -> embed $ bank_transactions <$> fromRight undefined <$> eitherDecodeFileStrict fp

runOutputOnCsv :: (Members '[Embed IO] r) => FilePath -> Sem (Output S.ByteString ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output csv -> embed $ S.writeFile fp csv

runapp outfile sem = runM . runOutputOnCsv outfile . runTransactionsProviderOnFile
  ("/Users" </> "shanedrury" </> "repos" </> "htransaction" </> "sample.json") $ sem

app :: (Members '[Embed IO, Input [Transaction], Output S.ByteString] r) => Sem r ()
app = do
  transactions <- input
  let csv = CSV.encode transactions
  output csv

args :: Parser Args
args = Args <$> strOption
  (  long "outfile"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "Out file" )

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  options <- execParser opts
  runapp (outfile options) app
