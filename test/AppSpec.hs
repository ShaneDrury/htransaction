module AppSpec
  ( spec,
  )
where

import Api
import App (AppM (..), runApp, syncTransactions)
import Cli
import Data.Maybe (fromJust)
import Data.Time
import Db
import Fa
import Interpretation.Http
import Interpretation.OAuth
import Logger
import qualified Monzo as MZ
import qualified Network.HTTP.Req as H
import Polysemy
import Polysemy.Cached
import Polysemy.Db
import Polysemy.Error
import Polysemy.Http
import Polysemy.Input
import Polysemy.LastImported
import Polysemy.Output
import Polysemy.State
import Test.Hspec
import Token
import Transaction
import Types
import Prelude

app :: (Members '[AppM] r) => Sem r ()
app = syncTransactions

runTransactionsManagerEmpty :: InterpreterFor TransactionsManager r
runTransactionsManagerEmpty = interpret $ \case
  GetNewTransactions _ -> return []

runTransactionsManagerSimple :: [Transaction] -> InterpreterFor TransactionsManager r
runTransactionsManagerSimple txs = interpret $ \case
  GetNewTransactions _ -> return txs

runDbEmpty :: InterpreterFor DbM r
runDbEmpty = interpret $ \case
  FindByUuid _ -> return Nothing
  InsertUnique _ -> return Nothing
  GetAccount _ -> return Nothing
  UpdateLastImported _ _ -> return ()
  GetTokensByInstitution _ -> return Nothing
  UpsertTokens tkns -> return tkns
  GetClient _ -> return Nothing

runAppEmpty :: ([LogMsg], ())
runAppEmpty =
  ( run
      . runOutputList @LogMsg
      . runLoggerAsOutput
      . runShowTransactionsMEmpty
      . runTransactionsManagerEmpty
      . runInputConst testArgs
      . runInputConst testConfig
      . runApp
  )
    app

runAppSimple :: [Transaction] -> ([LogMsg], ([[Transaction]], ()))
runAppSimple transactions =
  ( run
      . runOutputList @LogMsg
      . runLoggerAsOutput
      . runOutputList @[Transaction]
      . runTransactionsManagerSimple transactions
      . runInputConst testArgs
      . runInputConst testConfig
      . runApp
  )
    app

testTokenExpiresAt :: UTCTime
testTokenExpiresAt = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2020-3-09"

expiredTokenTime :: UTCTime
expiredTokenTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2020-3-01"

testCurrentTime :: UTCTime
testCurrentTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2020-3-04"

testConfig :: BankAccount
testConfig = BankAccount {bankAccountReference = "1", bankAccountLastImported = LastImported $ fromGregorian 2020 4 19, bankAccountInstitution = Fa}

testConfigMonzo :: BankAccount
testConfigMonzo = BankAccount {bankAccountReference = "2", bankAccountLastImported = LastImported $ fromGregorian 2020 4 19, bankAccountInstitution = Monzo}

testTokens :: TokenSet
testTokens =
  TokenSet
    { tokenSetAccessToken = (AccessToken "token"),
      tokenSetRefreshToken = (RefreshToken "refreshToken"),
      tokenSetExpiresAt = testTokenExpiresAt,
      tokenSetInstitution = Fa
    }

testTokensMonzo :: TokenSet
testTokensMonzo =
  testTokens
    { tokenSetInstitution = Monzo
    }

refreshTokenEndpoint :: TokenEndpoint
refreshTokenEndpoint =
  TokenEndpoint
    { access_token = AccessToken "accessTokenRefresh",
      token_type = "foo",
      expires_in = 86400,
      refresh_token = RefreshToken "refreshTokenRefresh",
      refresh_token_expires_in = Just 123
    }

accessTokenEndpoint :: TokenEndpoint
accessTokenEndpoint =
  TokenEndpoint
    { access_token = AccessToken "accessTokenAccess",
      token_type = "foo",
      expires_in = 86400,
      refresh_token = RefreshToken "refreshTokenAccess",
      refresh_token_expires_in = Just 123
    }

authorizationCode :: AuthorizationCode
authorizationCode = AuthorizationCode "foo"

transactionsEndpoint :: [Transaction] -> TransactionsEndpoint
transactionsEndpoint tx =
  TransactionsEndpoint
    { bank_transactions = tx2fa <$> tx
    }

tx2monzo :: Transaction -> MZ.MonzoTransaction
tx2monzo Transaction {..} =
  MZ.MonzoTransaction
    { created = UTCTime (fromGregorian 2020 4 20) (0 :: DiffTime),
      description = "Foo",
      amount = 12300,
      id = "asdf",
      metadata = MZ.MonzoMetadata {original_transaction_id = Nothing},
      merchant = Just $ MZ.MonzoMerchant {MZ.name = "merchant_name"},
      notes = "",
      declined_reason = Nothing
    }

monzoTransactionsEndpoint :: [Transaction] -> MZ.MonzoTransactionsEndpoint
monzoTransactionsEndpoint tx =
  MZ.MonzoTransactionsEndpoint
    { transactions = tx2monzo <$> tx
    }

tx2fa :: Transaction -> FaTransaction
tx2fa Transaction {..} =
  FaTransaction
    { dated_on = dated_on,
      description = description,
      amount = amount
    }

testTransactions :: [Transaction]
testTransactions =
  [ Transaction
      { dated_on = TransactionDate $ fromGregorian 2020 4 20,
        description = "Foo",
        amount = "123.0",
        comment = Nothing
      },
    Transaction
      { dated_on = TransactionDate $ fromGregorian 2020 4 21,
        description = "Bar",
        amount = "456.0",
        comment = Nothing
      }
  ]

runHttpMTest :: forall v r. Members '[Input (Maybe (Maybe [Transaction])), Error ApiError, AccessTokenM] r => ([Transaction] -> v) -> InterpreterFor (HttpM v) r
runHttpMTest convert =
  interpret
    ( \case
        RunRequest {} -> do
          _ <- getAccessToken
          mmtxs <- input @(Maybe (Maybe [Transaction]))
          case mmtxs of
            Just mtxs ->
              case mtxs of
                Just txs -> return $ Right $ convert txs
                Nothing -> return $ Left $ Unauthorized (H.JsonHttpException "foo")
            Nothing -> return $ Right $ convert []
    )

testArgs :: Args
testArgs = Args {Cli.bankAccountId = "1", outfile = "", verbose = True, dbFile = ""}

testArgsMonzo :: Args
testArgsMonzo = Args {Cli.bankAccountId = "2", outfile = "", verbose = True, dbFile = ""}

runAppDeep ::
  [Maybe [Transaction]] ->
  BankAccount ->
  Args ->
  TokenSet ->
  Either AppError ([LogMsg], ([LastImported], ([[Transaction]], ([Maybe TokenSet], ([[MZ.MonzoTransaction]], ())))))
runAppDeep tx bankaccount args tokens =
  ( run
      . handleErrors
      . runOutputList @LogMsg
      . runLoggerAsOutput
      . runInputConst args
      . runInputConst bankaccount
      . runPersistLastImportedMList
      . runApiTokenMConst refreshTokenEndpoint accessTokenEndpoint authorizationCode
      . runInputList tx
      . runOutputList @[Transaction]
      . runInputConst testCurrentTime
      . runInputConst @(Maybe TokenSet) (Just tokens)
      . runOutputList @(Maybe TokenSet)
      . runStateCached @(Maybe TokenSet)
      . saveTokens
      . runAccessTokenM
      . runHttpMTest @MZ.MonzoTransactionsEndpoint monzoTransactionsEndpoint
      . runHttpMTest @TransactionsEndpoint transactionsEndpoint
      . runApiHttpMOnTokens @MZ.MonzoTransactionsEndpoint
      . runApiHttpMOnTokens @TransactionsEndpoint
      . retryOnUnauthorized @MZ.MonzoTransactionsEndpoint
      . retryOnUnauthorized @TransactionsEndpoint
      . runFaM
      . runDbEmpty
      . runOutputList @[MZ.MonzoTransaction]
      . MZ.runMonzoM
      . runTransactionsApiM
      . runTransactionsManager
      . runApp
  )
    app

spec :: Spec
spec = do
  describe "AccessTokenM" $ do
    let tokenApp :: (Members '[AccessTokenM] r) => Sem r AccessToken
        tokenApp = getAccessToken
    context "happy path" $ do
      let happyRunner ::
            BankAccount ->
            Maybe TokenSet ->
            AccessToken
          happyRunner ba tokens =
            ( run
                . runApiTokenMConst refreshTokenEndpoint accessTokenEndpoint authorizationCode
                . runInputConst ba
                . runInputConst tokens
                . evalState tokens
                . runInputConst testCurrentTime
                . runAccessTokenM
            )
              tokenApp
      it "uses the config token if not expired" $
        happyRunner testConfig (Just testTokens) `shouldBe` AccessToken "token"
      it "uses the refresh token if expired" $
        happyRunner testConfig (Just $ testTokens {tokenSetExpiresAt = expiredTokenTime}) `shouldBe` AccessToken "accessTokenRefresh"
      it "uses the access token if token not present in config" $
        happyRunner testConfig (Nothing) `shouldBe` AccessToken "accessTokenAccess"
  describe "app" $ do
    context "empty path" $
      it "imports nothing" $
        runAppEmpty `shouldBe` ([(Info, "Number of transactions: 0")], ())
    context "happy, simple path" $
      context "with two transactions" $
        it "imports them" $
          runAppSimple testTransactions
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
                    amount = "123.0",
                    comment = Nothing
                  }
              )
      it "imports 100 transaction and produces a warning" $
        case runAppDeep [Just transactions_100] testConfig testArgs testTokens of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [(Info, "Getting transactions after 2020-04-19"), (Warning, "WARNING: Number of transactions close to limit"), (Info, "Number of transactions: 100")],
                           ( [LastImported $ fromGregorian 2020 4 20],
                             ( [ transactions_100
                               ],
                               ( [],
                                 ([], ())
                               )
                             )
                           )
                         )
    context "happy, deep path" $ do
      it "imports transactions, outputs them, updates config" $
        case runAppDeep [Just testTransactions] testConfig testArgs testTokens of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                             (Info, "Number of transactions: 2")
                           ],
                           ( [LastImported $ fromGregorian 2020 4 21],
                             ( [ testTransactions
                               ],
                               ( [],
                                 ([], ())
                               )
                             )
                           )
                         )
      it "imports 0 transactions" $
        case runAppDeep [Just []] testConfig testArgs testTokens of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            r
              `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                             (Info, "Number of transactions: 0")
                           ],
                           ([], ([[]], ([], ([], ()))))
                         )
      it "tries to refresh tokens if needed" $
        case runAppDeep [Just testTransactions] testConfig testArgs (testTokens {tokenSetExpiresAt = expiredTokenTime}) of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig =
                  testTokens
                    { tokenSetAccessToken = (AccessToken "accessTokenRefresh"),
                      tokenSetRefreshToken = (RefreshToken "refreshTokenRefresh"),
                      tokenSetExpiresAt = (addUTCTime (86400 :: NominalDiffTime) testCurrentTime)
                    }
             in r
                  `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                                 (Info, "Number of transactions: 2")
                               ],
                               ( [LastImported $ fromGregorian 2020 4 21],
                                 ( [ testTransactions
                                   ],
                                   ([Just updatedTokenConfig], ([], ()))
                                 )
                               )
                             )
      it "retries if unauthorized" $
        case runAppDeep [Nothing, Just testTransactions] testConfig testArgs testTokens of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig =
                  testTokens
                    { tokenSetAccessToken = (AccessToken "accessTokenRefresh"),
                      tokenSetRefreshToken = (RefreshToken "refreshTokenRefresh"),
                      tokenSetExpiresAt =
                        ( addUTCTime
                            ( 86400 :: NominalDiffTime
                            )
                            testCurrentTime
                        )
                    }
             in r
                  `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                                 (LogError, "Unauthorized"),
                                 (Info, "Number of transactions: 2")
                               ],
                               ( [LastImported $ fromGregorian 2020 4 21],
                                 ( [ testTransactions
                                   ],
                                   ( [ Just updatedTokenConfig
                                     ],
                                     ([], ())
                                   )
                                 )
                               )
                             )
      it "retries if unauthorized for Monzo" $
        case runAppDeep [Nothing, Just testTransactions] testConfigMonzo testArgsMonzo testTokensMonzo of
          Left e -> expectationFailure $ "expected Right, got Left: " ++ show e
          Right r ->
            let updatedTokenConfig =
                  testTokensMonzo
                    { tokenSetAccessToken = (AccessToken "accessTokenRefresh"),
                      tokenSetRefreshToken = (RefreshToken "refreshTokenRefresh"),
                      tokenSetExpiresAt =
                        ( addUTCTime
                            ( 86400 :: NominalDiffTime
                            )
                            testCurrentTime
                        )
                    }
             in r
                  `shouldBe` ( [ (Info, "Getting transactions after 2020-04-19"),
                                 (LogError, "Unauthorized"),
                                 (Info, "Number of transactions: 2")
                               ],
                               ( [LastImported $ fromGregorian 2020 4 20],
                                 ( [[Transaction {dated_on = TransactionDate $ fromGregorian 2020 4 20, description = "merchant_name", amount = "123.0", comment = Just "Foo"}, Transaction {dated_on = TransactionDate $ fromGregorian 2020 4 20, description = "merchant_name", amount = "123.0", comment = Just "Foo"}]],
                                   ( [ Just updatedTokenConfig
                                     ],
                                     ([tx2monzo <$> testTransactions], ())
                                   )
                                 )
                               )
                             )
      it "only retries once if unauthorized" $
        case runAppDeep [Nothing, Nothing] testConfigMonzo testArgsMonzo testTokens of
          Left (AppApiError (Unauthorized _)) -> return ()
          Left e -> expectationFailure $ "expected Left AppApiError, got Left: " ++ show e
          Right r -> expectationFailure $ "expected Left, got Right: " ++ show r
