{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module AppSpec
  ( spec,
  )
where

import App (app)
import Config
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Tagged
import Data.Time
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Cached
import Polysemy.Config
import Polysemy.Error
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace
import Test.Hspec
import Token
import Transaction
import Types
import Prelude

runTransactionsManagerEmpty :: Sem (TransactionsManager : r) a -> Sem r a
runTransactionsManagerEmpty = interpret $ \case
  GetTransactions -> return []
  OutputTransactions _ -> return ()

runTransactionsManagerSimple :: (Members '[Output [Transaction]] r) => [Transaction] -> Sem (TransactionsManager : r) a -> Sem r a
runTransactionsManagerSimple txs = interpret $ \case
  GetTransactions -> return txs
  OutputTransactions tx -> output tx

runAppEmpty :: Sem '[Output LastImported, TransactionsManager, Trace] () -> ([String], ([LastImported], ()))
runAppEmpty =
  run
    . runTraceList
    . runTransactionsManagerEmpty
    . runOutputList @LastImported

runAppSimple :: [Transaction] -> Sem '[TransactionsManager, Output [Transaction], Output LastImported, Trace] () -> ([String], ([LastImported], ([[Transaction]], ())))
runAppSimple transactions =
  run
    . runTraceList
    . runOutputList @LastImported
    . runOutputList @([Transaction])
    . runTransactionsManagerSimple transactions

testTime :: Maybe UTCTime
testTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04"

testCurrentTime :: UTCTime
testCurrentTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04"

testConfig :: Config
testConfig =
  Config
    { _bankAccounts = (Map.fromList [(1, LastImported $ fromGregorian 2020 4 19)]),
      _token = Just "token",
      _refreshToken = Just "refreshToken",
      _clientID = "clientID",
      _clientSecret = "secret",
      _tokenExpiresAt = testTime
    }

refreshTokenEndpoint :: Tagged Refresh TokenEndpoint
refreshTokenEndpoint =
  Tagged @Refresh $
    TokenEndpoint
      { access_token = "asdf",
        token_type = "foo",
        expires_in = 123,
        refresh_token = Just "foo"
      }

accessTokenEndpoint :: Tagged AccessToken TokenEndpoint
accessTokenEndpoint =
  Tagged @AccessToken $
    TokenEndpoint
      { access_token = "asdf",
        token_type = "foo",
        expires_in = 123,
        refresh_token = Just "foo"
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
      }
  ]

runApiManager :: [Transaction] -> Sem (ApiManager : r) a -> Sem r a
runApiManager tx = interpret $ \case
  GetApiTransactions _ _ -> return $ transactionsEndpoint tx

runAppDeep :: [Transaction] -> Sem '[TransactionsManager, Output [Transaction], Input [Transaction], ApiManager, LastImportedManager, Input ValidToken, Input (Tagged AccessToken TokenEndpoint), Input UTCTime, Input (Tagged Refresh TokenEndpoint), Output LastImported, LastImportedManager, Input LastImported, BankAccountsM, ConfigM, State (Cached Config), Output Config, Input Config, Trace, Error H.HttpException, Error AppError] a -> Either AppError ([String], ([Config], ([[Transaction]], a)))
runAppDeep tx =
  run
    . handleErrors
    . runTraceList
    . runInputConst testConfig
    . runOutputList @Config
    . runCached @Config
    . runConfigM
    . runBankAccountsMOnConfig
    . runGetLastImported 1
    . runLastImportedManager
    . runOutputLastImportedOnFile 1
    . runInputConst refreshTokenEndpoint
    . runInputConst testCurrentTime
    . runSaveRefreshTokens
    . runInputConst accessTokenEndpoint
    . runSaveAccessTokens
    . runValidToken
    . runLastImportedManager
    . runApiManager tx
    . runInputOnApi 1
    . retryOnUnauthorized
    . runOutputList @([Transaction])
    . runTransactionsManager

spec :: Spec
spec = do
  describe "app" $ do
    context "empty path" $ do
      it "imports nothing" $ do
        (runAppEmpty app) `shouldBe` (["Number of transactions: 0"], ([], ()))
    context "happy, simple path" $ do
      context "with one transaction" $ do
        it "imports 1 transaction" $ do
          (runAppSimple testTransactions app)
            `shouldBe` ( ["Number of transactions: 1"],
                         ( [ LastImported $ fromGregorian 2020 4 20
                           ],
                           ( [ testTransactions
                             ],
                             ()
                           )
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
      it "imports 100 transaction and produces a warning" $ do
        (runAppSimple transactions_100 app)
          `shouldBe` ( ["Number of transactions: 100", "WARNING: Number of transactions close to limit"],
                       ( [ LastImported $ fromGregorian 2020 4 20
                         ],
                         ( [ transactions_100
                           ],
                           ()
                         )
                       )
                     )
    context "happy, deep path" $ do
      it "imports transactions, outputs them, updates config" $ do
        case runAppDeep testTransactions app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ "Getting transactions from 1 after 2020-04-19",
                             "Number of transactions: 1",
                             "Outputting last imported day of LastImported 2020-04-20"
                           ],
                           ( [ updateConfig 1 (LastImported $ fromGregorian 2020 4 20) testConfig
                             ],
                             ( [ testTransactions
                               ],
                               ()
                             )
                           )
                         )
      it "imports 0 transactions" $ do
        case runAppDeep [] app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ "Getting transactions from 1 after 2020-04-19",
                             "Number of transactions: 0"
                           ],
                           ([], ([[]], ()))
                         )
