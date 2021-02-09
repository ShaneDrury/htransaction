module Main
  ( main,
  )
where

import Api
import App
import Cli
import Colog.Polysemy.Effect
import Config hiding (bankAccountId)
import Control.Monad
import Data.Time.Clock
import qualified Db as DB
import Fa
import Interpretation.Http
import Interpretation.OAuth
import Logger
import Monzo
import Network.HTTP.Req
import Polysemy
import Polysemy.BankAccount
import Polysemy.Cached
import Polysemy.Config
import Polysemy.Error
import Polysemy.Http
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.OAuth
import Polysemy.Output
import Polysemy.Random
import Polysemy.State
import Polysemy.Trace
import Token
import Transaction
import Types
import Prelude

runapp ::
  Args ->
  BankInstitution ->
  Sem
    '[ AppM,
       TransactionsManager,
       TransactionsApiM,
       Output [Transaction],
       MonzoM,
       Output [MonzoTransaction],
       DB.DbM,
       FaM,
       ApiHttpM TransactionsEndpoint,
       ApiHttpM MonzoTransactionsEndpoint,
       AccessTokenM,
       OAuthM,
       HttpM TransactionsEndpoint,
       HttpM MonzoTransactionsEndpoint,
       HttpM TokenEndpoint,
       Input UTCTime,
       RandomM,
       PersistLastImportedM,
       State TokenSet,
       BankAccountsM,
       Input Args,
       State Config,
       Log LogMsg,
       Trace,
       Error HttpException,
       Error ApiError,
       Error AppError,
       Embed IO
     ]
    a ->
  IO (Either AppError a)
runapp args@Args {..} institution =
  runM
    . handleErrors
    . runOutputOnLog verbose
    . runLoggerOnRainbow
    . runGetConfig configFile
    . runWriteConfig configFile
    . runStateCached @Config
    . runInputConst args
    . runBankAccountsMOnConfig
    . runGetTokens tokensFile -- Input TokenSet
    . runWriteTokens tokensFile
    . runStateCached @TokenSet
    . runPersistLastImportedM
    . runRandomROnIO
    . runGetTime
    . runHttpMOnReq @TokenEndpoint
    . runHttpMOnReq @MonzoTransactionsEndpoint
    . runHttpMOnReq @TransactionsEndpoint
    . runOAuthMOnInstitution institution
    . runAccessTokenM
    . runApiHttpMOnTokens @MonzoTransactionsEndpoint
    . runApiHttpMOnTokens @TransactionsEndpoint
    . runFaM
    . DB.runDbMOnSqlite dbFile
    . outputMonzoOnDb
    . runMonzoM
    . saveTokens
    . retryOnUnauthorized @MonzoTransactionsEndpoint
    . retryOnUnauthorized @TransactionsEndpoint
    . runOutputOnCsv outfile
    . runTransactionsApiM
    . runTransactionsManager
    . runApp
    . runWithDb dbFile

runSync :: (Members '[AppM] r) => Sem r ()
runSync = syncTransactions

getStaticInstitution :: Args -> IO BankInstitution
getStaticInstitution args@Args {..} =
  ( runM
      . runLoggerOnRainbow
      . runOutputOnLog verbose
      . runGetConfig configFile
      . runWriteConfig configFile
      . runStateCached @Config
      . runInputConst args
      . runBankAccountsMOnConfig
  )
    getInstitution

main :: IO ()
main = do
  options <- getArgs
  institution <- getStaticInstitution options
  result <- runapp options institution runSync
  case result of
    Left e -> print e
    Right () -> return ()
