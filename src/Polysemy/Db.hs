{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Db
  ( DbM (..),
    findByUuid,
    insertUnique,
    getAccount,
    updateLastImported,
    getTokensByInstitution,
    upsertTokens,
    getClient,
  )
where

import Data.Text
import Database.Esqueleto.Experimental hiding (get, insertUnique)
import Db
import Polysemy
import Types
import Prelude

data DbM m a where
  FindByUuid :: Text -> DbM m (Maybe MonzoTransaction)
  InsertUnique :: MonzoTransaction -> DbM m (Maybe (Key MonzoTransaction))
  GetAccount :: Text -> DbM m (Maybe BankAccount)
  UpdateLastImported :: Text -> LastImported -> DbM m ()
  GetTokensByInstitution :: BankInstitution -> DbM m (Maybe TokenSet)
  UpsertTokens :: TokenSet -> DbM m TokenSet
  GetClient :: BankInstitution -> DbM m (Maybe Client)

$(makeSem ''DbM)
