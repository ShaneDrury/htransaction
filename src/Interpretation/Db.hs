module Interpretation.Db
  ( runDbMOnSqlite,
    runMigrations,
  )
where

import Data.Text
import Database.Esqueleto.Experimental hiding (get, insertUnique)
import qualified Database.Persist.Sqlite as P
import Db
import Polysemy
import Polysemy.Db
import Types
import Prelude

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

updateTokens_ :: TokenSet -> SqlPersistM (Entity TokenSet)
updateTokens_ tokenSet@TokenSet {..} =
  upsert
    tokenSet
    [ TokenSetAccessToken P.=. tokenSetAccessToken,
      TokenSetRefreshToken P.=. tokenSetRefreshToken,
      TokenSetExpiresAt P.=. tokenSetExpiresAt
    ]

runDbMOnSqlite :: (Members '[Embed IO] r) => FilePath -> InterpreterFor DbM r
runDbMOnSqlite fp = interpret $ \case
  FindByUuid uuid -> runQ $ do
    r <- findByUuid_ uuid
    return $ entityVal <$> r
  InsertUnique tx -> runQ (P.insertUnique tx)
  GetAccount ref -> runQ $ do
    account <- accountByReference ref
    return $ entityVal <$> account
  UpdateLastImported bankAccountId lastImported -> runQ $ updateLastImported_ bankAccountId lastImported
  GetTokensByInstitution institution -> runQ $ do
    results <- getTokensByInstitution_ institution
    return $ entityVal <$> results
  UpsertTokens tokens -> runQ $ do
    r <- updateTokens_ tokens
    return $ entityVal r
  GetClient institution -> runQ $ do
    result <- selectFirst [ClientInstitution P.==. institution] []
    return $ entityVal <$> result
  where
    runQ :: Members '[Embed IO] r => SqlPersistM a -> Sem r a
    runQ qry = embed $ P.runSqlite (pack fp) qry

runMigrations :: (Members '[Embed IO] r) => FilePath -> Sem r ()
runMigrations fp = embed $ P.runSqlite (pack fp) (runMigration migrateAll)
