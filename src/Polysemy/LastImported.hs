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
  ( GetLastImportedM (..),
    PersistLastImportedM (..),
    getLastImported,
    runLastImportedManager,
    persistLastImported,
    runPersistLastImportedM,
    runPersistLastImportedMList,
  )
where

import Config
import Control.Lens
import Control.Monad
import qualified Data.Map as Map
import Data.Time
import Logger
import Polysemy
import Polysemy.Config
import Polysemy.Output
import Types
import Prelude

data GetLastImportedM m a where
  GetLastImported :: GetLastImportedM m Day

data PersistLastImportedM m a where
  PersistLastImported :: Day -> PersistLastImportedM m ()

$(makeSem ''GetLastImportedM)

$(makeSem ''PersistLastImportedM)

runLastImportedManager :: (Members '[BankAccountsM, ConfigM, Logger] r) => Int -> InterpreterFor GetLastImportedM r
runLastImportedManager bankAccountId = interpret $ \case
  GetLastImported -> do
    bas <- getBankAccounts
    case Map.lookup bankAccountId bas of
      Just ba -> do
        let (LastImported day) = ba ^. lastImported
        return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId

runPersistLastImportedM :: (Members '[BankAccountsM, ConfigM, Logger, GetLastImportedM] r) => Int -> InterpreterFor PersistLastImportedM r
runPersistLastImportedM bankAccountId = interpret $ \case
  PersistLastImported day -> do
    originalConfig <- getConfig
    originalDay <- getLastImported
    when (day /= originalDay) $ do
      info $ "Outputting last imported day of " ++ show day
      writeConfig (updateConfig bankAccountId (LastImported day) originalConfig)

runPersistLastImportedMList :: Sem (PersistLastImportedM : r) a -> Sem r ([LastImported], a)
runPersistLastImportedMList =
  runOutputList @LastImported
    . reinterpret
      ( \case
          PersistLastImported day -> output (LastImported day)
      )
