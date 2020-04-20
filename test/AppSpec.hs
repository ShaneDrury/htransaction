{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module AppSpec
  ( spec,
  )
where

import App (app)
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

runOutputLastImportedEmpty :: Sem ((Output LastImported) : r) a -> Sem r a
runOutputLastImportedEmpty = interpret $ \case
  Output _ -> return ()

runAppEmpty =
  runM
    . runTraceList
    . runTransactionsManagerEmpty
    . runOutputLastImportedEmpty

spec :: Spec
spec = do
  describe "app" $ do
    context "empty path" $ do
      it "imports nothing" $ do
        (runAppEmpty app) `shouldBe` [(["Number of transactions: 0"], ())]
