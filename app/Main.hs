{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Polysemy.Config
import Polysemy.Embed
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import System.Exit
import Token
import Transaction
import Types

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
