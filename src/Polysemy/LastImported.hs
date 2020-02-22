{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.LastImported
  ( runOutputLastImportedOnStdout,
    runOutputLastImportedOnFile,
    runGetLastImported,
  )
where

import Config
import Control.Lens
import Control.Monad
import qualified Data.Map as Map
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import Types
import Prelude

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[Input LastImported, Input Config, Output Config, Trace] r) => FilePath -> Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- input
    originalDay <- input
    when (day /= originalDay) $ do
      trace $ "Outputting last imported day of " ++ show day
      output (updateConfig bankAccountId day originalConfig)

runGetLastImported :: (Members '[Input Config] r) => Int -> Sem (Input LastImported ': r) a -> Sem r a
runGetLastImported bankAccountId = interpret $ \case
  Input -> do
    cfg <- input @Config
    case Map.lookup bankAccountId (cfg ^. bankAccounts) of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId
