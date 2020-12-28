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
import Monzo
import Polysemy
import Polysemy.BankAccount
import Polysemy.Cached
import Polysemy.Config
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Random
import Token
import Transaction
import Types
import Prelude

runapp args@Args {..} =
  runM
    . handleErrors
    . runOutputOnLog verbose
    . runLoggerOnRainbow
    . runGetConfig configFile
    . runWriteConfig configFile
    . runStateCached @Config
    . runInputConst args
    . runBankAccountsMOnConfig
    . runGetTokens tokensFile
    . runWriteTokens tokensFile
    . runStateCached @Tokens
    . runPersistLastImportedM
    . runRandomROnIO
    . runApiTokenM
    . runGetTime
    . saveTokens
    . runGetToken
    . runValidToken
    . runFaM @TransactionsEndpoint
    . runMonzoM @MonzoTransactionsEndpoint
    . retryOnUnauthorized
    . runOutputOnCsv outfile
    . runTransactionsApiM
    . runTransactionsManager

main :: IO ()
main = do
  options <- getArgs
  result <- runapp options app
  case result of
    Left e -> print e
    Right () -> return ()
