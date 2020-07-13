{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( Args (..),
    getArgs,
  )
where

import GHC.Generics
import Options.Generic
import Prelude

data Args
  = Args
      { outfile :: FilePath,
        configFile :: FilePath,
        tokensFile :: FilePath,
        verbose :: Bool,
        bankAccountId :: Int
      }
  deriving (Eq, Generic, Show)

instance ParseRecord Args

getArgs :: IO Args
getArgs = getRecord "htransaction"
