module Polysemy.BankAccount
  ( runBankAccountsMOnDb,
    getInstitution,
  )
where

import Cli
import Config
import Data.Maybe
import Data.Text
import Db
import Polysemy
import Polysemy.Input
import Prelude

runBankAccountsMOnDb :: (Members '[Input Args, DbM] r) => InterpreterFor (Input BankAccount) r
runBankAccountsMOnDb = interpret $ \case
  Input -> do
    args <- input @Args
    fromJust <$> getAccount (pack $ Cli.bankAccountId args)

getInstitution :: (Members '[Input BankAccount] r) => Sem r BankInstitution
getInstitution = bankAccountInstitution <$> input
