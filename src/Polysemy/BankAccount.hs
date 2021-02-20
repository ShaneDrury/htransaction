module Polysemy.BankAccount
  ( runBankAccountsMOnConfig,
    getInstitution,
    getLastImported,
    getBankAccountFromConfig,
    getInstitutionStatic,
  )
where

import Cli
import Config
import Control.Lens
import Data.List (find)
import Data.Maybe
import Polysemy
import Polysemy.Input
import Polysemy.State
import Types
import Prelude

runBankAccountsMOnConfig :: (Members '[State Config, Input Args] r) => InterpreterFor (Input BankAccount) r
runBankAccountsMOnConfig = interpret $ \case
  Input -> getBankAccountFromConfig <$> get <*> input

getBankAccountFromConfig :: Config -> Args -> BankAccount
getBankAccountFromConfig config args = fromJust $ find (\ba -> ba ^. Config.bankAccountId == Cli.bankAccountId args) (config ^. bankAccounts)

getInstitutionStatic :: Config -> Args -> BankInstitution
getInstitutionStatic config args = getBankAccountFromConfig config args ^. bankInstitution

getInstitution :: (Members '[Input BankAccount] r) => Sem r BankInstitution
getInstitution = (^. bankInstitution) <$> input

getLastImported :: (Members '[Input BankAccount] r) => Sem r LastImported
getLastImported = (^. lastImported) <$> input
