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
import Control.Lens
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

runAppEmpty :: Sem '[Output LastImported, TransactionsManager, Logger, Output LogMsg] () -> ([LogMsg], ([LastImported], ()))
runAppEmpty =
  run
    . runOutputList @LogMsg
    . runLoggerAsOutput
    . runTransactionsManagerEmpty
    . runOutputList @LastImported

runAppSimple :: [Transaction] -> Sem '[TransactionsManager, Output [Transaction], Output LastImported, Logger, Output LogMsg] () -> ([LogMsg], ([LastImported], ([[Transaction]], ())))
runAppSimple transactions =
  run
    . runOutputList @LogMsg
    . runLoggerAsOutput
    . runOutputList @LastImported
    . runOutputList @([Transaction])
    . runTransactionsManagerSimple transactions

testTokenExpiresAt :: Maybe UTCTime
testTokenExpiresAt = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-09"

expiredTokenTime :: Maybe UTCTime
expiredTokenTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-01"

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
      _tokenExpiresAt = testTokenExpiresAt
    }

refreshTokenEndpoint :: Tagged Refresh TokenEndpoint
refreshTokenEndpoint =
  Tagged @Refresh $
    TokenEndpoint
      { access_token = "accessTokenRefresh",
        token_type = "foo",
        expires_in = 86400,
        refresh_token = Just "refreshTokenRefresh"
      }

accessTokenEndpoint :: Tagged AccessToken TokenEndpoint
accessTokenEndpoint =
  Tagged @AccessToken $
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
      }
  ]

runApiManager :: Members '[Input (Maybe (Maybe [Transaction])), Input ValidToken, Error H.HttpException] r => Sem (ApiManager : r) a -> Sem r a
runApiManager =
  interpret $
    ( \case
        GetApiTransactions _ _ -> do
          input @ValidToken
          mmtxs <- input @(Maybe (Maybe [Transaction]))
          case mmtxs of
            Just mtxs ->
              case mtxs of
                Just txs -> return $ Right $ transactionsEndpoint txs
                Nothing -> (return $ Left Unauthorized)
            Nothing -> error "huh"
    )

runAppDeep :: [Maybe [Transaction]] -> Config -> Sem '[TransactionsManager, Output [Transaction], Input (Either ApiError [Transaction]), ApiManager, Input (Maybe (Maybe [Transaction])), LastImportedManager, Input ValidToken, Input (Tagged AccessToken TokenEndpoint), Input UTCTime, Input (Tagged Refresh TokenEndpoint), Output LastImported, LastImportedManager, Input LastImported, BankAccountsM, ConfigM, State (Cached Config), Output Config, Input Config, Logger, Output LogMsg, Error H.HttpException, Error ApiError, Error AppError] a -> Either AppError ([LogMsg], ([Config], ([[Transaction]], a)))
runAppDeep tx config =
  run
    . handleErrors
    . runOutputList @LogMsg
    . runLoggerAsOutput
    . runInputConst config
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
    . runInputList tx
    . runApiManager
    . runInputOnApi 1
    . retryOnUnauthorized
    . runOutputList @([Transaction])
    . runTransactionsManager

spec :: Spec
spec = do
  describe "app" $ do
    context "empty path" $ do
      it "imports nothing" $ do
        (runAppEmpty app) `shouldBe` ([(Info, "Number of transactions: 0")], ([], ()))
    context "happy, simple path" $ do
      context "with one transaction" $ do
        it "imports 1 transaction" $ do
          (runAppSimple testTransactions app)
            `shouldBe` ( [(Info, "Number of transactions: 1")],
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
          `shouldBe` ( [(Info, "Number of transactions: 100"), (Warning, "WARNING: Number of transactions close to limit")],
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
        case runAppDeep [Just testTransactions] testConfig app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ (Info, "Getting transactions from 1 after 2020-04-19"),
                             (Info, "Number of transactions: 1"),
                             (Info, "Outputting last imported day of LastImported 2020-04-20")
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
        case runAppDeep [Just []] testConfig app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ (Info, "Getting transactions from 1 after 2020-04-19"),
                             (Info, "Number of transactions: 0")
                           ],
                           ([], ([[]], ()))
                         )
      it "tries to refresh tokens if needed" $ do
        case runAppDeep [Just testTransactions] (testConfig & tokenExpiresAt .~ expiredTokenTime) app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig = testConfig &~ do
                  token .= Just "accessTokenRefresh"
                  refreshToken .= Just "refreshTokenRefresh"
                  tokenExpiresAt .= Just ((addUTCTime (secondsToNominalDiffTime 86400)) $ testCurrentTime)
                updatedLastImportConfig = updatedTokenConfig & bankAccounts .~ (Map.singleton 1 (LastImported $ fromGregorian 2020 4 20))
             in r
                  `shouldBe` ( [ (Info, "Getting transactions from 1 after 2020-04-19"),
                                 (Info, "Number of transactions: 1"),
                                 (Info, "Outputting last imported day of LastImported 2020-04-20")
                               ],
                               ( [ updatedTokenConfig,
                                   updatedLastImportConfig
                                 ],
                                 ( [ testTransactions
                                   ],
                                   ()
                                 )
                               )
                             )
      it "retries if unauthorized" $ do
        case runAppDeep [Nothing, Just testTransactions] testConfig app of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig = testConfig &~ do
                  token .= Just "accessTokenRefresh"
                  refreshToken .= Just "refreshTokenRefresh"
                  tokenExpiresAt .= Just ((addUTCTime (secondsToNominalDiffTime 86400)) $ testCurrentTime)
                updatedLastImportConfig = updatedTokenConfig & bankAccounts .~ (Map.singleton 1 (LastImported $ fromGregorian 2020 4 20))
             in r
                  `shouldBe` ( [ (Info, "Getting transactions from 1 after 2020-04-19"),
                                 (LogError, "Unauthorized"),
                                 (Info, "Getting transactions from 1 after 2020-04-19"),
                                 (Info, "Number of transactions: 1"),
                                 (Info, "Outputting last imported day of LastImported 2020-04-20")
                               ],
                               ( [ updatedTokenConfig,
                                   updatedLastImportConfig
                                 ],
                                 ( [ testTransactions
                                   ],
                                   ()
                                 )
                               )
                             )
