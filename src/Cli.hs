module Cli
  ( Args (..),
    getArgs,
  )
where

import GHC.Generics
import Options.Generic
import Prelude

data Args = Args
  { outfile :: FilePath,
    verbose :: Bool,
    bankAccountId :: String,
    dbFile :: FilePath
  }
  deriving stock (Eq, Generic, Show)

instance ParseRecord Args

getArgs :: IO Args
getArgs = getRecord "htransaction"
