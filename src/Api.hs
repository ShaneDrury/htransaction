{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( TransactionsApiM (..),
    getTransactionsApi,
    runTransactionsApiM,
    TransactionsEndpoint (..),
  )
where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Time
import Fa
import GHC.Generics (Generic)
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Types
import Prelude hiding (log)

data TransactionsApiM m a where
  GetTransactionsApi :: Int -> Day -> TransactionsApiM m [Transaction]

$(makeSem ''TransactionsApiM)

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

runTransactionsApiM :: (Members '[FaM TransactionsEndpoint, Error ApiError] r) => InterpreterFor TransactionsApiM r
runTransactionsApiM = interpret $ \case
  GetTransactionsApi bankAccountId fromDate -> do
    etx <-
      getFa
        "bank_transactions"
        ( "bank_account" =: bankAccountId
            <> "from_date" =: fromDate
            <> "sort" =: ("dated_on" :: Text)
            <> "per_page" =: (100 :: Int)
        )
    case etx of
      Right r -> return $ bank_transactions r
      Left e -> throw e
