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
    . runOutputOnCsv outfile
    . runGetTime
    . runStateCached
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
