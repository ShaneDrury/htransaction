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
  Sem
    '[ AppM,
       TransactionsManager,
       TransactionsApiM,
       Output [Transaction],
       Output [MonzoTransaction],
       DB.DbM,
       MonzoM MonzoTransactionsEndpoint,
       FaM TransactionsEndpoint,
       ApiHttpM TransactionsEndpoint,
       ApiHttpM MonzoTransactionsEndpoint,
       HttpM TransactionsEndpoint,
       HttpM MonzoTransactionsEndpoint,
       AccessTokenM,
       OAuthM,
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
runapp args@Args {..} =
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
    . runOAuthM
    . saveTokens
    . runAccessTokenM
    . runHttpMOnReq @MonzoTransactionsEndpoint
    . runHttpMOnReq @TransactionsEndpoint
    . runApiHttpMOnTokens @MonzoTransactionsEndpoint
    . runApiHttpMOnTokens @TransactionsEndpoint
    . runFaM @TransactionsEndpoint
    . runMonzoM @MonzoTransactionsEndpoint
    . DB.runDbMOnSqlite dbFile
    . outputMonzoOnDb
    . outputMonzoTransactions
    . retryOnUnauthorized
    . runOutputOnCsv outfile
    . runTransactionsApiM
    . runTransactionsManager
    . runApp
    . runWithDb dbFile

runSync :: (Members '[AppM] r) => Sem r ()
runSync = syncTransactions

main :: IO ()
main = do
  options <- getArgs
  result <- runapp options runSync
  case result of
    Left e -> print e
    Right () -> return ()
