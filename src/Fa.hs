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

module Fa
  ( FaM (..),
    getFa,
    runFaM,
    retryOnUnauthorized,
  )
where

import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Text
import qualified Network.HTTP.Client as H
import Network.HTTP.Req
import qualified Network.HTTP.Types.Status as Status
import Polysemy
import Polysemy.Error
import Token
import Types
import Prelude hiding (log)

data FaM v m a where
  GetFa :: Text -> Option 'Https -> FaM v m (Either ApiError v)

$(makeSem ''FaM)

faRequest :: (MonadHttp m, FromJSON a) => Text -> ValidToken -> Option 'Https -> m (JsonResponse a)
faRequest endpoint (ValidToken token) options =
  req
    GET
    (https "api.freeagent.com" /: "v2" /: endpoint)
    NoReqBody
    jsonResponse
    ( oAuth2Bearer token
        <> header
          "User-Agent"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
        <> options
    )

$(makePrisms ''HttpException) -- req

$(makePrisms ''H.HttpException)

$(makePrisms ''H.HttpExceptionContent)

responseP :: Traversal' HttpException (H.Response ())
responseP = _VanillaHttpException . _HttpExceptionRequest . _2 . _StatusCodeException . _1

statusP :: HttpException -> Maybe Status.Status
statusP e = H.responseStatus <$> e ^? responseP

isUnauthorized :: HttpException -> Bool
isUnauthorized e = case statusP e of
  Just status -> status == Status.unauthorized401
  Nothing -> False

runFaM ::
  forall v r.
  ( Members
      '[ Embed IO,
         Error HttpException,
         ValidTokenM
       ]
      r,
    FromJSON v
  ) =>
  InterpreterFor (FaM v) r
runFaM = interpret $ \case
  GetFa endpoint options -> do
    token <- getValidToken
    result <- embed $ E.try $ do
      r <- runReq defaultHttpConfig $ faRequest endpoint token options
      return (responseBody r :: v)
    case result of
      Right res -> return $ Right (res :: v)
      Left err' ->
        if isUnauthorized err'
          then return $ Left Unauthorized
          else throw @HttpException err'

retryOnUnauthorized ::
  forall v r a.
  ( Members
      '[ Logger,
         FaM v,
         ValidTokenM
       ]
      r
  ) =>
  Sem r a ->
  Sem r a
retryOnUnauthorized =
  intercept @(FaM v)
    ( \case
        GetFa endpoint option -> do
          r <- getFa @v endpoint option
          case r of
            Left Unauthorized -> do
              err "Unauthorized"
              invalidateTokens
              getFa @v endpoint option
            s -> return s
    )
