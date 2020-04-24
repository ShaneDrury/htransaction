{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Config
  ( runGetConfig,
    runGetConfigTest,
    runWriteConfig,
    getConfig,
    ConfigM,
    BankAccountsM,
    getBankAccounts,
    runBankAccountsMOnConfig,
    runConfigM,
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
import Types
import Prelude

data ConfigM m a where
  GetConfig :: ConfigM m Config

$(makeSem ''ConfigM)

runConfigM :: (Members '[Input Config] r) => Sem (ConfigM : r) a -> Sem r a
runConfigM = interpret $ \case
  GetConfig -> input @Config

data BankAccountsM m a where
  GetBankAccounts :: BankAccountsM m BankAccounts

$(makeSem ''BankAccountsM)

runBankAccountsMOnConfig :: (Members '[ConfigM] r) => Sem (BankAccountsM : r) a -> Sem r a
runBankAccountsMOnConfig = interpret $ \case
  GetBankAccounts -> (^. bankAccounts) <$> getConfig

runGetConfig :: (Members '[Logger, Embed IO] r) => FilePath -> Sem (Input Config ': r) a -> Sem r a
runGetConfig fp = interpret $ \case
  Input -> do
    info $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runWriteConfig :: (Members '[Logger, Embed IO] r) => FilePath -> Sem (Output Config ': r) a -> Sem r a
runWriteConfig fp = interpret $ \case
  Output cfg -> do
    info $ "Writing config to " ++ fp
    embed $ S.writeFile fp (encode cfg)

runGetConfigTest :: Sem (Input Config ': r) a -> Sem r a
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
