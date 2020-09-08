{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module AppSpec
  ( spec,
  )
where

import Api
import App (app)
import Config
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Time
import Fa
import Logger
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Config
import Polysemy.Error
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import Polysemy.State
import Test.Hspec
import Token
import Transaction
import Types
import Prelude

runTransactionsManagerEmpty :: InterpreterFor TransactionsManager r
runTransactionsManagerEmpty = interpret $ \case
  GetNewTransactions -> return []

runTransactionsManagerSimple :: [Transaction] -> InterpreterFor TransactionsManager r
runTransactionsManagerSimple txs = interpret $ \case
  GetNewTransactions -> return txs

runAppEmpty :: Sem '[TransactionsManager, Output [Transaction], Logger] () -> ([LogMsg], ())
runAppEmpty =
  run
    . runOutputList @LogMsg
    . runLoggerAsOutput
    . runShowTransactionsMEmpty
    . runTransactionsManagerEmpty

runAppSimple :: [Transaction] -> Sem '[TransactionsManager, Output [Transaction], Logger] () -> ([LogMsg], ([[Transaction]], ()))
runAppSimple transactions =
  run
    . runOutputList @LogMsg
    . runLoggerAsOutput
    . runOutputList @[Transaction]
    . runTransactionsManagerSimple transactions

testTokenExpiresAt :: Maybe UTCTime
testTokenExpiresAt = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2020-3-09"

expiredTokenTime :: Maybe UTCTime
expiredTokenTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2020-3-01"

testCurrentTime :: UTCTime
testCurrentTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2020-3-04"

testConfig :: Config
testConfig =
  Config
    { _bankAccounts = [BankAccount {_bankAccountId = 1, _lastImported = LastImported $ fromGregorian 2020 4 19}]
    }

testTokens :: Tokens
testTokens =
  Tokens
    { _token = Just "token",
      _refreshToken = Just "refreshToken",
      _clientID = "clientID",
      _clientSecret = "secret",
      _tokenExpiresAt = testTokenExpiresAt
    }

refreshTokenEndpoint :: TokenEndpoint
refreshTokenEndpoint =
  TokenEndpoint
    { access_token = "accessTokenRefresh",
      token_type = "foo",
      expires_in = 86400,
      refresh_token = Just "refreshTokenRefresh"
    }

accessTokenEndpoint :: TokenEndpoint
accessTokenEndpoint =
  TokenEndpoint
    { access_token = "accessTokenAccess",
      token_type = "foo",
      expires_in = 86400,
      refresh_token = Just "refreshTokenAccess"
    }

transactionsEndpoint :: [Transaction] -> TransactionsEndpoint
transactionsEndpoint tx =
  TransactionsEndpoint
    { bank_transactions = tx
    }

testTransactions :: [Transaction]
testTransactions =
  [ Transaction
      { dated_on = TransactionDate $ fromGregorian 2020 4 20,
        description = "Foo",
        amount = "123.0"
      },
    Transaction
      { dated_on = TransactionDate $ fromGregorian 2020 4 21,
        description = "Bar",
        amount = "456.0"
      }
  ]

runFaMTest :: Members '[Input (Maybe (Maybe [Transaction])), Error ApiError, ValidTokenM] r => InterpreterFor (FaM TransactionsEndpoint) r
runFaMTest =
  interpret
    ( \case
        GetFa _ _ -> do
          getValidToken
          mmtxs <- input @(Maybe (Maybe [Transaction]))
          case mmtxs of
            Just mtxs ->
              case mtxs of
                Just txs -> return $ Right $ transactionsEndpoint txs
                Nothing -> return $ Left Unauthorized
            Nothing -> return $ Right $ transactionsEndpoint []
    )

runAppDeep ::
  [Maybe [Transaction]] ->
  Config ->
  Tokens ->
  Sem
    '[ TransactionsManager,
       TransactionsApiM,
       FaM TransactionsEndpoint,
       ValidTokenM,
       TokenM,
       Input UTCTime,
       TokensM,
       Output [Transaction],
       Input (Maybe (Maybe [Transaction])),
       ApiTokenM,
       PersistLastImportedM,
       GetLastImportedM,
       BankAccountsM,
       ConfigM,
       Logger,
       Error H.HttpException,
       Error ApiError,
       Error AppError
     ]
    a ->
  Either AppError ([LogMsg], ([Config], ([[Transaction]], ([Tokens], a))))
runAppDeep tx config tokens =
  run
    . handleErrors
    . runOutputList @LogMsg
    . runLoggerAsOutput
    . runInputConst config
    . runOutputList @Config
    . runStateCached @Config
    . runConfigM
    . runBankAccountsMOnConfig
    . runLastImportedManager 1
    . runPersistLastImportedM 1
    . runApiTokenMConst refreshTokenEndpoint accessTokenEndpoint
    . runInputList tx
    . runOutputList @[Transaction]
    . runInputConst tokens
    . runOutputList @Tokens
    . runStateCached @Tokens
    . runTokensM
    . runInputConst testCurrentTime
    . saveTokens
    . runGetToken
    . runValidToken
    . runFaMTest
    . retryOnUnauthorized @TransactionsEndpoint
    . runTransactionsApiM
    . runTransactionsManager 1

spec :: Spec
spec = do
  describe "runValidToken" $ do
    let tokenApp :: (Members '[ValidTokenM] r) => Sem r ValidToken
        tokenApp = getValidToken
    context "happy path" $ do
      let happyRunner ::
            Config ->
            Tokens ->
            Sem
              '[ ValidTokenM,
                 TokenM,
                 Input UTCTime,
                 TokensM,
                 Input Tokens,
                 ConfigM,
                 Input Config,
                 ApiTokenM
               ]
              ValidToken ->
            ValidToken
          happyRunner config tokens =
            run
              . runApiTokenMConst refreshTokenEndpoint accessTokenEndpoint
              . runInputConst config
              . evalState config
              . runConfigM
              . runInputConst tokens
              . evalState tokens
              . runTokensM
              . runInputConst testCurrentTime
              . runGetToken
              . runValidToken
      it "uses the config token if not expired" $
        happyRunner testConfig testTokens tokenApp `shouldBe` ValidToken (BS.pack "token")
      it "uses the refresh token if expired" $
        happyRunner testConfig (testTokens & tokenExpiresAt .~ expiredTokenTime) tokenApp `shouldBe` ValidToken (BS.pack "accessTokenRefresh")
      it "uses the access token if tokenExpiresAt is not set" $
        happyRunner testConfig (testTokens & tokenExpiresAt .~ Nothing) tokenApp `shouldBe` ValidToken (BS.pack "accessTokenAccess")
      it "uses the access token if token not present in config" $
        happyRunner testConfig (testTokens & token .~ Nothing) tokenApp `shouldBe` ValidToken (BS.pack "accessTokenAccess")
  describe "app" $ do
    context "empty path"
      $ it "imports nothing"
      $ runAppEmpty app `shouldBe` ([(Info, "Number of transactions: 0")], ())
    context "happy, simple path"
      $ context "with two transactions"
      $ it "imports them"
      $ runAppSimple testTransactions app
        `shouldBe` ( [(Info, "Number of transactions: 2")],
                     ( [ testTransactions
                       ],
                       ()
                     )
                   )
    context "with 100 transactions" $ do
      let transactions_100 =
            replicate
              100
              ( Transaction
                  { dated_on = TransactionDate $ fromGregorian 2020 4 20,
                    description = "Foo",
                    amount = "123.0"
                  }
              )
      it "imports 100 transaction and produces a warning" $
        runAppSimple transactions_100 app
          `shouldBe` ( [(Info, "Number of transactions: 100"), (Warning, "WARNING: Number of transactions close to limit")],
                       ( [ transactions_100
                         ],
                         ()
                       )
                     )
    context "happy, deep path" $ do
      it "imports transactions, outputs them, updates config" $
        case runAppDeep [Just testTransactions] testConfig testTokens app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                             (Info, "Outputting last imported day of 2020-04-21"),
                             (Info, "Number of transactions: 2")
                           ],
                           ( [updateConfig 1 (LastImported $ fromGregorian 2020 4 21) testConfig],
                             ( [ testTransactions
                               ],
                               ( [],
                                 ()
                               )
                             )
                           )
                         )
      it "imports 0 transactions" $
        case runAppDeep [Just []] testConfig testTokens app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                             (Info, "Number of transactions: 0")
                           ],
                           ([], ([[]], ([], ())))
                         )
      it "tries to refresh tokens if needed" $
        case runAppDeep [Just testTransactions] testConfig (testTokens & tokenExpiresAt .~ expiredTokenTime) app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig = testTokens &~ do
                  token .= Just "accessTokenRefresh"
                  refreshToken .= Just "refreshTokenRefresh"
                  tokenExpiresAt .= Just (addUTCTime (86400 :: NominalDiffTime) testCurrentTime)
                updatedLastImportConfig = testConfig & bankAccounts .~ [BankAccount {_bankAccountId = 1, _lastImported = LastImported $ fromGregorian 2020 4 21}]
             in r
                  `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                                 (Info, "Outputting last imported day of 2020-04-21"),
                                 (Info, "Number of transactions: 2")
                               ],
                               ( [ updatedLastImportConfig
                                 ],
                                 ( [ testTransactions
                                   ],
                                   ([updatedTokenConfig], ())
                                 )
                               )
                             )
      it "retries if unauthorized" $
        case runAppDeep [Nothing, Just testTransactions] testConfig testTokens app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig = testTokens &~ do
                  token .= Just "accessTokenRefresh"
                  refreshToken .= Just "refreshTokenRefresh"
                  tokenExpiresAt
                    .= Just
                      ( addUTCTime
                          ( 86400 :: NominalDiffTime
                          )
                          testCurrentTime
                      )
                updatedLastImportConfig = testConfig & bankAccounts .~ [BankAccount {_bankAccountId = 1, _lastImported = LastImported $ fromGregorian 2020 4 21}]
             in r
                  `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                                 (LogError, "Unauthorized"),
                                 (Info, "Outputting last imported day of 2020-04-21"),
                                 (Info, "Number of transactions: 2")
                               ],
                               ( [ updatedLastImportConfig
                                 ],
                                 ( [ testTransactions
                                   ],
                                   ( [ testTokens & tokenExpiresAt ?~ testCurrentTime,
                                       updatedTokenConfig
                                     ],
                                     ()
                                   )
                                 )
                               )
                             )
