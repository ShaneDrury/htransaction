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
  ( PersistLastImportedM (..),
    persistLastImported,
    runPersistLastImportedM,
    runPersistLastImportedMList,
  )
where

import Config
import Control.Lens
import Control.Monad
import Data.Time
import Logger
import Polysemy
import Polysemy.BankAccount
import Polysemy.Config
import Polysemy.Output
import Polysemy.State
import Types
import Prelude

data PersistLastImportedM m a where
  PersistLastImported :: Day -> PersistLastImportedM m ()

$(makeSem ''PersistLastImportedM)

runPersistLastImportedM :: (Members '[BankAccountsM, State Config, Logger] r) => InterpreterFor PersistLastImportedM r
runPersistLastImportedM = interpret $ \case
  PersistLastImported day -> do
    bankAccount <- getBankAccount
    originalConfig <- get @Config
    let (LastImported originalDay) = bankAccount ^. lastImported
    when (day /= originalDay) $ do
      info $ "Outputting last imported day of " ++ show day
      put (updateConfig (bankAccount ^. bankAccountId) (LastImported day) originalConfig)

runPersistLastImportedMList :: Sem (PersistLastImportedM : r) a -> Sem r ([LastImported], a)
runPersistLastImportedMList =
  runOutputList @LastImported
    . reinterpret
      ( \case
          PersistLastImported day -> output (LastImported day)
      )
