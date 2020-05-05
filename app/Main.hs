{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main,
  )
where

import App
import Cli
import Control.Monad
import Polysemy
import Config
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
    . runUseRefreshTokens
    . runGetAccessTokens
    . runGetTime
    . runSaveRefreshTokens
    . runSaveAccessTokens
    . runGetToken
    . runValidToken
    . runFaM
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
