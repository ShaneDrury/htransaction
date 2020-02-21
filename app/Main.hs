{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Cli
import Config
import Control.Monad
import Data.Tagged
import Polysemy
import Polysemy.Config
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
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

runapp ::
  Args ->
  Sem
    '[ Input [Transaction],
       Input ValidToken,
       Input (Tagged AccessToken TokenEndpoint),
       Input LastImported,
       Input (Tagged Refresh TokenEndpoint),
       Output TokenEndpoint,
       Output [Transaction],
       Output LastImported,
       Output Message,
       Input Config,
       Embed IO
     ]
    a ->
  IO a
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
