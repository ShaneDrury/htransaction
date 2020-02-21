{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cli where

import GHC.Generics
import Options.Generic

data Args
  = Args
      { outfile :: FilePath,
        configFile :: FilePath,
        verbose :: Bool,
        bankAccountId :: Int
      }
  deriving (Eq, Generic, Show)

instance ParseRecord Args

getArgs :: IO Args
getArgs = getRecord "htransaction"
