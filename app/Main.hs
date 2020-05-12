{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Api
import App
import Cli
import Config
import Control.Monad
import Fa
import Logger
import Polysemy
import Polysemy.Config
import Polysemy.LastImported
import Token
import Transaction
import Types
import Prelude

runapp Args {..} =
  runM
    . handleErrors
    . runOutputOnLog verbose
    . runLoggerOnRainbow
    . runGetConfig configFile
    . runWriteConfig configFile
    . runStateCached @Config
    . runConfigM
    . runBankAccountsMOnConfig
    . runLastImportedManager bankAccountId
    . runPersistLastImportedM bankAccountId
    . runApiTokenM
    . runGetTime
    . saveTokens
    . runGetToken
    . runValidToken
    . runFaM @TransactionsEndpoint
    . retryOnUnauthorized
    . runOutputOnCsv outfile
    . runTransactionsApiM
    . runTransactionsManager bankAccountId
    . runNextTransactionsMOnLastImported

main :: IO ()
main = do
  options <- getArgs
  result <- runapp options app
  case result of
    Left e -> print e
    Right () -> return ()
