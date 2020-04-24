{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import App
import Cli
import Config
import Control.Monad
import Polysemy
import Polysemy.Cached
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
    . runLoggerAsTrace
    . runGetConfig configFile
    . runWriteConfig configFile
    . runCached @Config
    . runOutputOnCsv outfile
    . runGetTime
    . runConfigM
    . runBankAccountsMOnConfig
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runLastImportedManager
    . runOutputLastImportedOnFile bankAccountId
    . runGetAccessTokens
    . runSaveRefreshTokens
    . runSaveAccessTokens
    . runValidToken
    . runApiManagerOnNetwork
    . runLastImportedManager
    . runInputOnApi bankAccountId
    . retryOnUnauthorized
    . runTransactionsManager

main :: IO ()
main = do
  options <- getArgs
  result <- runapp options app
  case result of
    Left e -> print e
    Right () -> return ()
