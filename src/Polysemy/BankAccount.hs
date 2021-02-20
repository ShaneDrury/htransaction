{-# LANGUAGE TemplateHaskell #-}

module Polysemy.BankAccount
  ( runBankAccountsMOnConfig,
    getInstitution,
    getLastImported,
  )
where

import Cli
import Config
import Control.Lens
import Control.Monad
import Data.List (find)
import Data.Maybe
import Polysemy
import Polysemy.Input
import Polysemy.State
import Types
import Prelude

runBankAccountsMOnConfig :: (Members '[State Config, Input Args] r) => InterpreterFor (Input BankAccount) r
runBankAccountsMOnConfig = interpret $ \case
  Input -> do
    config <- get @Config
    args <- input @Args
    let baList = config ^. bankAccounts
    return $ fromJust $ find (\ba -> ba ^. Config.bankAccountId == Cli.bankAccountId args) baList

getInstitution :: (Members '[Input BankAccount] r) => Sem r BankInstitution
getInstitution = (^. bankInstitution) <$> input

getLastImported :: (Members '[Input BankAccount] r) => Sem r LastImported
getLastImported = (^. lastImported) <$> input
