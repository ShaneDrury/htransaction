{-# LANGUAGE TemplateHaskell #-}

module Polysemy.BankAccount
  ( BankAccountsM (..),
    getBankAccount,
    runBankAccountsMOnConfig,
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
import Prelude

data BankAccountsM m a where
  GetBankAccount :: BankAccountsM m BankAccount

$(makeSem ''BankAccountsM)

runBankAccountsMOnConfig :: (Members '[State Config, Input Args] r) => InterpreterFor BankAccountsM r
runBankAccountsMOnConfig = interpret $ \case
  GetBankAccount -> do
    config <- get @Config
    args <- input @Args
    let baList = config ^. bankAccounts
    return $ fromJust $ find (\ba -> ba ^. Config.bankAccountId == Cli.bankAccountId args) baList
