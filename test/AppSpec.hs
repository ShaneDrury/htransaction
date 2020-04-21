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
import Data.Time
import Polysemy
import Polysemy.Output
import Polysemy.Trace
import Test.Hspec
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

spec :: Spec
spec = do
  describe "app" $ do
    context "empty path" $ do
      it "imports nothing" $ do
        (runAppEmpty app) `shouldBe` (["Number of transactions: 0"], ([], ()))
    context "happy, simple path" $ do
      context "with one transaction" $ do
        let transactions =
              [ Transaction
                  { dated_on = TransactionDate $ fromGregorian 2020 4 20,
                    description = "Foo",
                    amount = "123.0"
                  }
              ]
        it "imports 1 transaction" $ do
          (runAppSimple transactions app)
            `shouldBe` ( ["Number of transactions: 1"],
                         ( [ LastImported $ fromGregorian 2020 4 20
                           ],
                           ( [ transactions
                             ],
                             ()
                           )
                         )
                       )
    context "with 100 transactions" $ do
      let transactions =
            replicate
              100
              ( Transaction
                  { dated_on = TransactionDate $ fromGregorian 2020 4 20,
                    description = "Foo",
                    amount = "123.0"
                  }
              )
      it "imports 100 transaction and produces a warning" $ do
        (runAppSimple transactions app)
          `shouldBe` ( ["Number of transactions: 100", "WARNING: Number of transactions close to limit"],
                       ( [ LastImported $ fromGregorian 2020 4 20
                         ],
                         ( [ transactions
                           ],
                           ()
                         )
                       )
                     )
