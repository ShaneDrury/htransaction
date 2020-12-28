{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Monzo where

import qualified Control.Exception as E
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Time.Clock
import GHC.Generics (Generic)
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Request
import Token
import Types
import Prelude hiding (log)

data MonzoM v m a where
  GetMonzo :: Text -> Option 'Https -> MonzoM v m (Either ApiError v)

$(makeSem ''MonzoM)

data MonzoTransaction
  = MonzoTransaction
      { amount :: Int,
        description :: String,
        created :: UTCTime
      }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype MonzoTransactionsEndpoint
  = MonzoTransactionsEndpoint
      { transactions :: [MonzoTransaction]
      }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

monzoRequest :: (MonadHttp m, FromJSON a) => Text -> ValidToken -> Option 'Https -> m (JsonResponse a)
monzoRequest endpoint (ValidToken token) options =
  req
    GET
    (https "api.monzo.com" /: endpoint)
    NoReqBody
    jsonResponse
    ( oAuth2Bearer token
        <> header
          "User-Agent"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
        <> options
    )

runMonzoM ::
  forall v r.
  ( Members
      '[ Embed IO,
         Error HttpException,
         ValidTokenM
       ]
      r,
    FromJSON v
  ) =>
  InterpreterFor (MonzoM v) r
runMonzoM = interpret $ \case
  GetMonzo endpoint options -> do
    token <- getValidToken
    result <- embed $ E.try $ do
      r <- runReq defaultHttpConfig $ monzoRequest endpoint token options
      return (responseBody r :: v)
    case result of
      Right res -> return $ Right (res :: v)
      Left err' ->
        if isUnauthorized err'
          then return $ Left Unauthorized
          else throw @HttpException err'
