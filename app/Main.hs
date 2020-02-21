{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Cli
import Config
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Csv as CSV
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Tagged
import Data.Text hiding (drop, length, null)
import Data.Time
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import System.Exit
import Token
import Transaction
import Types

runInputTest :: (Members '[Embed IO] r) => Sem (Input [Transaction] ': r) a -> Sem r a
runInputTest = interpret $ \case
  Input -> return []

runOutputOnCsv :: (Members '[Embed IO, Output Message] r) => FilePath -> Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    log' $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

runOutputOnStdout :: (Members '[Embed IO] r) => Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnStdout = interpret $ \case
  Output tx -> embed $ S.putStrLn (CSV.encode tx)

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[Embed IO, Input Config, Output Message] r) => FilePath -> Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- input
    log' $ "Writing last imported day of " ++ show day ++ " to " ++ fp
    embed $ S.writeFile fp (encode (updateConfig bankAccountId day originalConfig))

runInputOnNetwork :: (Members '[Embed IO, Input LastImported, Output Message, Input ValidToken] r) => Int -> Sem (Input [Transaction] ': r) a -> Sem r a
runInputOnNetwork bankAccountId = interpret $ \case
  Input -> do
    (LastImported fromDate) <- input
    token <- input @ValidToken
    log' $ "Getting transactions from " ++ show bankAccountId ++ " after " ++ show fromDate
    embed $ bank_transactions <$> getTransactions bankAccountId fromDate token

runGetLastImported :: (Members '[Input Config] r) => Int -> Sem (Input LastImported ': r) a -> Sem r a
runGetLastImported bankAccountId = interpret $ \case
  Input -> do
    cfg <- input @Config
    case Map.lookup bankAccountId (cfg ^. bankAccounts) of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId

app :: (Members '[Input [Transaction], Output [Transaction], Output LastImported, Output Message] r) => Sem r ()
app = do
  tx <- input
  log' $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (log' "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  output tx

runapp Args {..} =
  runM
    . runGetConfig configFile
    . runOutputOnLog verbose
    . runOutputLastImportedOnFile configFile bankAccountId
    . runOutputOnCsv outfile
    . runSaveTokens configFile
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runGetAccessTokens
    . runValidToken
    . runInputOnNetwork bankAccountId

main :: IO ()
main = do
  options <- getArgs
  runapp options app
