module Main
  ( main,
  )
where

import Api
import App
import Cli
import Control.Monad
import qualified Db as DB
import Fa
import Interpretation.Http
import Interpretation.OAuth
import Logger
import Monzo
import Polysemy
import Polysemy.BankAccount
import Polysemy.Cached
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Random
import Token
import Transaction
import Types
import Prelude

runapp ::
  Args ->
  BankInstitution ->
  IO (Either AppError ())
runapp args@Args {..} institution =
  ( runM
      . handleErrors
      . runLoggerOnRainbow
      . runInputConst args
      . DB.runDbMOnSqlite dbFile
      . runBankAccountsMOnDb
      . runGetTime
      . runGetTokens
      . runWriteTokens
      . runStateCached @(Maybe DB.TokenSet)
      . runPersistLastImportedM
      . runRandomROnIO
      . runHttpMOnReq @TokenEndpoint
      . runHttpMOnReq @MonzoTransactionsEndpoint
      . runHttpMOnReq @TransactionsEndpoint
      . runInputClientOnDb
      . runOAuthMOnInstitution institution
      . saveTokens
      . runAccessTokenM
      . runApiHttpMOnTokens @MonzoTransactionsEndpoint
      . runApiHttpMOnTokens @TransactionsEndpoint
      . runFaM
      . outputMonzoOnDb
      . runMonzoM
      . retryOnUnauthorized @MonzoTransactionsEndpoint
      . retryOnUnauthorized @TransactionsEndpoint
      . runOutputOnCsv outfile
      . runTransactionsApiM
      . runTransactionsManager
      . runApp
  )
    runSync

runSync :: (Members '[AppM] r) => Sem r ()
runSync = syncTransactions

getInstitutionStatic :: Args -> IO BankInstitution
getInstitutionStatic args@Args {..} =
  ( runM
      . runInputConst args
      . DB.runDbMOnSqlite dbFile
      . runBankAccountsMOnDb
  )
    getInstitution

main :: IO ()
main = do
  options <- getArgs
  runM (DB.runMigrations (dbFile options))
  institution <- getInstitutionStatic options
  result <- runapp options institution
  case result of
    Left e -> print e
    Right () -> return ()
