{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.LastImported
  ( runOutputLastImportedOnStdout,
    runOutputLastImportedOnFile,
    runGetLastImported,
    LastImportedManager (..),
    getLastImported,
    runLastImportedManager,
  )
where

import Config
import Control.Monad
import qualified Data.Map as Map
import Polysemy
import Polysemy.Config
import Polysemy.Input
import Polysemy.Output
import Types
import Prelude

data LastImportedManager m a where
  GetLastImported :: LastImportedManager m LastImported

$(makeSem ''LastImportedManager)

runLastImportedManager :: (Members '[Input LastImported] r) => InterpreterFor LastImportedManager r
runLastImportedManager = interpret $ \case
  GetLastImported -> input @LastImported

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => InterpreterFor (Output LastImported) r
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[LastImportedManager, ConfigM, Output Config, Logger] r) => Int -> InterpreterFor (Output LastImported) r
runOutputLastImportedOnFile bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- getConfig
    originalDay <- getLastImported
    when (day /= originalDay) $ do
      info $ "Outputting last imported day of " ++ show day
      output (updateConfig bankAccountId day originalConfig)

runGetLastImported :: (Members '[BankAccountsM] r) => Int -> InterpreterFor (Input LastImported) r
runGetLastImported bankAccountId = interpret $ \case
  Input -> do
    bankAccounts <- getBankAccounts
    case Map.lookup bankAccountId bankAccounts of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId
