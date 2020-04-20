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
    LastImportedManager(..),
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
import Polysemy.Trace
import Types
import Prelude

data LastImportedManager m a where
  GetLastImported :: LastImportedManager m LastImported

$(makeSem ''LastImportedManager)

runLastImportedManager :: (Members '[Input LastImported] r) => Sem (LastImportedManager : r) a -> Sem r a
runLastImportedManager = interpret $ \case
  GetLastImported -> input @LastImported

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[LastImportedManager, ConfigM, Output Config, Trace] r) => Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- getConfig
    originalDay <- getLastImported
    when (day /= originalDay) $ do
      trace $ "Outputting last imported day of " ++ show day
      output (updateConfig bankAccountId day originalConfig)

runGetLastImported :: (Members '[BankAccountsM] r) => Int -> Sem (Input LastImported ': r) a -> Sem r a
runGetLastImported bankAccountId = interpret $ \case
  Input -> do
    bankAccounts <- getBankAccounts
    case Map.lookup bankAccountId bankAccounts of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId
