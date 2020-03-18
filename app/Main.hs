{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Cli
import Config
import Control.Monad
import Polysemy
import Polysemy.Cached
import Polysemy.Config
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import Polysemy.Trace
import Token
import Transaction
import Types
import Prelude

app :: (Members '[TransactionsManager, Output [Transaction], Output LastImported, Trace] r) => Sem r ()
app = do
  tx <- getTransactions
  trace $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (trace "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  output tx

runapp Args {..} =
  runM
    . handleErrors
    . runOutputOnLog verbose
    . runGetConfig configFile
    . runWriteConfig configFile
    . runCached @Config
    . runOutputOnCsv outfile
    . runGetTime
    . runSaveTokens
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runOutputLastImportedOnFile bankAccountId
    . runGetAccessTokens
    . tokenFromAccessToken
    . tokenFromRefreshToken
    . runInvalidToken
    . runValidToken
    . runInputOnNetwork bankAccountId
    . retryOnUnauthorized
    . runTransactionsManager

main :: IO ()
main = do
  options <- getArgs
  result <- runapp options app
  case result of
    Left e -> print e
    Right () -> return ()
