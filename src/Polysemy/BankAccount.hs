module Polysemy.BankAccount
  ( runBankAccountsMOnDb,
    getInstitution,
    runInputClientOnDb,
  )
where

import Cli
import Data.Maybe
import Data.Text
import Db
import Polysemy
import Polysemy.Input
import Types
import Prelude

runBankAccountsMOnDb :: (Members '[Input Args, DbM] r) => InterpreterFor (Input BankAccount) r
runBankAccountsMOnDb = interpret $ \case
  Input -> do
    args <- input @Args
    fromJust <$> getAccount (pack $ Cli.bankAccountId args)

getInstitution :: (Members '[Input BankAccount] r) => Sem r BankInstitution
getInstitution = bankAccountInstitution <$> input

runInputClientOnDb :: (Members '[DbM, Input BankAccount] r) => InterpreterFor (Input Client) r
runInputClientOnDb = interpret $ \case
  Input -> do
    institution <- getInstitution
    mclient <- getClient institution
    case mclient of
      Just client -> return client
      Nothing -> error $ "No client for institution: " ++ show institution
