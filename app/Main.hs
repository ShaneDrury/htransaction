{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Cli
import Control.Monad
import Polysemy
import Polysemy.Config
import Polysemy.LastImported
import Polysemy.Output
import Polysemy.Trace
import Token
import Transaction
import Types
import Prelude

app :: (Members '[TransactionsManager, Output LastImported, Trace] r) => Sem r ()
app = do
  tx <- getTransactions
  trace $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (trace "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  outputTransactions tx

runapp Args {..} =
  runM
    . handleErrors
    . runOutputOnLog verbose
    . runWriteConfig configFile
    . runOutputOnCsv outfile
    . runGetTime
    . runConfigM configFile
    . runCacheConfig
    . runBankAccountsMOnConfig
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runLastImportedManager
    . runOutputLastImportedOnFile bankAccountId
    . runGetAccessTokens
    . runSaveTokens
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
