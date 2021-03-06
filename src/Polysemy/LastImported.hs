{-# LANGUAGE TemplateHaskell #-}

module Polysemy.LastImported
  ( PersistLastImportedM (..),
    persistLastImported,
    runPersistLastImportedM,
    runPersistLastImportedMList,
  )
where

import Control.Monad
import Data.Time
import Db
import Logger
import Polysemy
import Polysemy.Db
import Polysemy.Input
import Polysemy.Output
import Types
import Prelude

data PersistLastImportedM m a where
  PersistLastImported :: Day -> PersistLastImportedM m ()

$(makeSem ''PersistLastImportedM)

runPersistLastImportedM :: (Members '[Input BankAccount, DbM, Logger] r) => InterpreterFor PersistLastImportedM r
runPersistLastImportedM = interpret $ \case
  PersistLastImported day -> do
    bankAccount <- input
    let (LastImported originalDay) = bankAccountLastImported bankAccount
    when (day /= originalDay) $ do
      info $ "Outputting last imported day of " ++ show day
      updateLastImported (bankAccountReference bankAccount) (LastImported day)

runPersistLastImportedMList :: Sem (PersistLastImportedM : r) a -> Sem r ([LastImported], a)
runPersistLastImportedMList =
  runOutputList @LastImported
    . reinterpret
      ( \case
          PersistLastImported day -> output (LastImported day)
      )
