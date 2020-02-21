{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cli
import Control.Monad
import Polysemy
import Polysemy.Config
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import Polysemy.Trace
import Prelude
import Token
import Transaction
import Types

app :: (Members '[Input [Transaction], Output [Transaction], Output LastImported, Trace] r) => Sem r ()
app = do
  tx <- input
  trace $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (trace "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  output tx

runapp Args {..} =
  runM
    . runGetConfig configFile
    . runOutputOnLog verbose
    . runOutputOnCsv outfile
    . runSaveTokens configFile
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runOutputLastImportedOnFile configFile bankAccountId
    . runGetAccessTokens
    . runValidToken
    . runInputOnNetwork bankAccountId

main :: IO ()
main = do
  options <- getArgs
  runapp options app
