{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

module Db
  ( MonzoTransaction (..),
    migrateAll,
    MonzoTransactionId,
    DbM (..),
    runDbMOnSqlite,
    findByUuid,
    insertUnique,
    runMigrations,
    getAccount,
    BankAccountId,
    BankAccount (..),
    updateLastImported,
    Client (..),
    TokenSet (..),
    getTokensByInstitution,
    TokenSetId,
    ClientId,
    updateTokens,
    getClient,
  )
where

import Data.Text
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (get, insertUnique)
import qualified Database.Persist.Sqlite as P
import Database.Persist.TH
import Polysemy
import Types
import Prelude

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
MonzoTransaction
    description Text
    amount Int
    uuid Text
    UniqueMonzoTransactionUuid uuid
    dateTime UTCTime
    originalTransactionId Text Maybe
    merchantName Text Maybe
    note Text
    deriving Eq Show

BankAccount
    institution BankInstitution
    lastImported LastImported
    reference Text
    deriving Eq Show

TokenSet
    accessToken AccessToken
    refreshToken RefreshToken
    expiresAt UTCTime
    institution BankInstitution
    deriving Eq Show

Client
    identifier Text
    secret Text
    institution BankInstitution
    deriving Eq Show
|]

data DbM m a where
  FindByUuid :: Text -> DbM m (Maybe MonzoTransaction)
  InsertUnique :: MonzoTransaction -> DbM m (Maybe (Key MonzoTransaction))
  GetAccount :: Text -> DbM m (Maybe BankAccount)
  UpdateLastImported :: Text -> LastImported -> DbM m ()
  GetTokensByInstitution :: BankInstitution -> DbM m (Maybe TokenSet)
  UpdateTokens :: BankInstitution -> TokenSet -> DbM m ()
  GetClient :: BankInstitution -> DbM m (Maybe Client)

$(makeSem ''DbM)

findByUuid_ :: Text -> SqlPersistM (Maybe (Entity MonzoTransaction))
findByUuid_ uuid = selectFirst [MonzoTransactionUuid P.==. uuid] []

accountByReference :: Text -> SqlPersistM (Maybe (Entity BankAccount))
accountByReference ref = selectFirst [BankAccountReference P.==. ref] []

updateLastImported_ :: Text -> LastImported -> SqlPersistM ()
updateLastImported_ baid lastImported = update $ \ba -> do
  set ba [BankAccountLastImported =. val lastImported]
  where_ (ba ^. BankAccountReference ==. val baid)

getTokensByInstitution_ :: BankInstitution -> SqlPersistM (Maybe (Entity TokenSet))
getTokensByInstitution_ institution = selectFirst [TokenSetInstitution P.==. institution] []

updateTokens_ :: BankInstitution -> TokenSet -> SqlPersistM ()
updateTokens_ institution TokenSet {..} = update $ \tokenset -> do
  set
    tokenset
    [ TokenSetAccessToken =. val tokenSetAccessToken,
      TokenSetRefreshToken =. val tokenSetRefreshToken,
      TokenSetExpiresAt =. val tokenSetExpiresAt
    ]
  where_ (tokenset ^. TokenSetInstitution ==. val institution)

runDbMOnSqlite :: (Members '[Embed IO] r) => FilePath -> InterpreterFor DbM r
runDbMOnSqlite fp = interpret $ \case
  FindByUuid uuid -> embed $ do
    r <- P.runSqlite (pack fp) (findByUuid_ uuid)
    return $ entityVal <$> r
  InsertUnique tx -> embed $ P.runSqlite (pack fp) (P.insertUnique tx)
  GetAccount ref -> embed $
    P.runSqlite (pack fp) $ do
      account <- accountByReference ref
      return $ entityVal <$> account
  UpdateLastImported bankAccountId lastImported -> embed $ P.runSqlite (pack fp) $ updateLastImported_ bankAccountId lastImported
  GetTokensByInstitution institution -> embed $
    P.runSqlite (pack fp) $ do
      results <- getTokensByInstitution_ institution
      return $ entityVal <$> results
  UpdateTokens institution tokens -> embed $ P.runSqlite (pack fp) $ updateTokens_ institution tokens
  GetClient institution -> embed $
    P.runSqlite (pack fp) $ do
      result <- selectFirst [ClientInstitution P.==. institution] []
      return $ entityVal <$> result

runMigrations :: (Members '[Embed IO] r) => FilePath -> Sem r ()
runMigrations fp = embed $ P.runSqlite (pack fp) (runMigration migrateAll)
