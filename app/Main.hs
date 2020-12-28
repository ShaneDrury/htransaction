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
import Fa
import Logger
import Monzo
import Network.HTTP.Req
import Polysemy
import Polysemy.BankAccount
import Polysemy.Cached
import Polysemy.Config
import Polysemy.Error
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
    '[ TransactionsManager,
       TransactionsApiM,
       Output [Transaction],
       MonzoM MonzoTransactionsEndpoint,
       FaM TransactionsEndpoint,
       ValidTokenM,
       TokenM,
       Input UTCTime,
       ApiTokenM,
       RandomM,
       PersistLastImportedM,
       State Tokens,
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
    . runGetTokens tokensFile
    . runWriteTokens tokensFile
    . runStateCached @Tokens
    . runPersistLastImportedM
    . runRandomROnIO
    . runApiTokenM
    . runGetTime
    . saveTokens
    . runGetToken
    . runValidToken
    . runFaM @TransactionsEndpoint
    . runMonzoM @MonzoTransactionsEndpoint
    . retryOnUnauthorized
    . runOutputOnCsv outfile
    . runTransactionsApiM
    . runTransactionsManager

main :: IO ()
main = do
  options <- getArgs
  result <- runapp options app
  case result of
    Left e -> print e
    Right () -> return ()
