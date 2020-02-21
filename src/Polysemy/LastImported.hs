{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.LastImported where

import Config
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Types

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[Embed IO, Input Config, Output Message] r) => FilePath -> Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- input
    log' $ "Writing last imported day of " ++ show day ++ " to " ++ fp
    embed $ S.writeFile fp (encode (updateConfig bankAccountId day originalConfig))

runGetLastImported :: (Members '[Input Config] r) => Int -> Sem (Input LastImported ': r) a -> Sem r a
runGetLastImported bankAccountId = interpret $ \case
  Input -> do
    cfg <- input @Config
    case Map.lookup bankAccountId (cfg ^. bankAccounts) of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId
