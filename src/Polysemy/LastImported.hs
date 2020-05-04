{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.LastImported
  ( runOutputLastImportedOnFile,
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

runLastImportedManager :: Sem (LastImportedManager : r) a -> Sem (Input LastImported : r) a
runLastImportedManager = reinterpret $ \case
  GetLastImported -> input @LastImported

runOutputLastImportedOnFile :: (Members '[LastImportedManager, ConfigM, Logger] r) => Int -> InterpreterFor (Output LastImported) r
runOutputLastImportedOnFile bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- getConfig
    originalDay <- getLastImported
    when (day /= originalDay) $ do
      info $ "Outputting last imported day of " ++ show day
      writeConfig (updateConfig bankAccountId day originalConfig)

runGetLastImported :: Int -> Sem (Input LastImported : r) a -> Sem (BankAccountsM : r) a
runGetLastImported bankAccountId = reinterpret $ \case
  Input -> do
    bas <- getBankAccounts
    case Map.lookup bankAccountId bas of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId
