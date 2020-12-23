{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import Data.List (find)
import Data.Maybe
import Data.Time
import Logger
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Types
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
