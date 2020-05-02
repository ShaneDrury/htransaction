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

module Polysemy.Config
  ( runGetConfig,
    runGetConfigTest,
    runWriteConfig,
    getConfig,
    writeConfig,
    ConfigM,
    BankAccountsM,
    getBankAccounts,
    runBankAccountsMOnConfig,
    runConfigM,
    runStateCached,
    Cached (..),
  )
where

import Config
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map
import Data.Time
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Types
import Prelude

data ConfigM m a where
  GetConfig :: ConfigM m Config
  WriteConfig :: Config -> ConfigM m ()

$(makeSem ''ConfigM)

runConfigM :: (Members '[State Config] r) => InterpreterFor ConfigM r
runConfigM = interpret $ \case
  GetConfig -> get @Config
  WriteConfig s -> put @Config s

data Cached a = Cached a | Dirty
  deriving stock (Eq, Ord, Show, Functor)

runStateCached :: forall v r a. (Members '[Input v, Output v] r) => Sem (State v : State (Cached v) : r) a -> Sem r a
runStateCached =
  evalState @(Cached v) Dirty
    . interpret
      ( \case
          Get -> do
            cachedConfig <- get @(Cached v)
            case cachedConfig of
              Dirty -> do
                cfg <- input @v
                put @(Cached v) (Cached cfg)
                return cfg
              Cached cfg -> return cfg
          Put cfg -> do
            output @v cfg
            put @(Cached v) (Cached cfg)
      )

data BankAccountsM m a where
  GetBankAccounts :: BankAccountsM m BankAccounts

$(makeSem ''BankAccountsM)

runBankAccountsMOnConfig :: (Members '[ConfigM] r) => InterpreterFor BankAccountsM r
runBankAccountsMOnConfig = interpret $ \case
  GetBankAccounts -> (^. bankAccounts) <$> getConfig

runGetConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Input Config) r
runGetConfig fp = interpret $ \case
  Input -> do
    info $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runWriteConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Output Config) r
runWriteConfig fp = interpret $ \case
  Output cfg -> do
    info $ "Writing config to " ++ fp
    embed $ S.writeFile fp (encode cfg)

runGetConfigTest :: InterpreterFor (Input Config) r
runGetConfigTest = interpret $ \case
  Input ->
    return $
      Config
        { _bankAccounts = Map.fromList [(123, LastImported $ fromGregorian 2020 02 04), (679673, LastImported $ fromGregorian 2020 02 07)],
          _token = Just "token",
          _refreshToken = Just "refresh",
          _clientID = "clientid",
          _clientSecret = "secret",
          _tokenExpiresAt = Nothing
        }
