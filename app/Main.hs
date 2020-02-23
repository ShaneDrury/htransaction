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

app :: (Members '[Input [Transaction], Output [Transaction], Output LastImported, Trace] r) => Sem r ()
app = do
  tx <- input
  trace $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (trace "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  output tx

runapp Args {..} =
  runM
    . runOutputOnLog verbose
    . runGetConfig configFile
    . runWriteConfig configFile
    . runCached @Config
    . runOutputOnCsv outfile
    . runGetTime
    . runSaveTokens
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runOutputLastImportedOnFile configFile bankAccountId
    . runGetAccessTokens
    . tokenFromAccessToken
    . tokenFromRefreshToken
    . runValidToken
    . runInputOnNetwork bankAccountId

main :: IO ()
main = do
  options <- getArgs
  runapp options app
