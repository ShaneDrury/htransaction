{-# LANGUAGE TemplateHaskell #-}

module Fa
  ( FaM (..),
    getFa,
    runFaM,
    retryOnUnauthorized,
  )
where

import qualified Control.Exception as E
import Control.Monad
import Data.Aeson
import Data.Text
import Logger
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Request
import Token
import Types
import Prelude hiding (log)

data FaM v m a where
  GetFa :: Text -> Option 'Https -> FaM v m (Either ApiError v)

$(makeSem ''FaM)

faRequest :: (MonadHttp m, FromJSON a) => Text -> ValidToken -> Option 'Https -> m (JsonResponse a)
faRequest endpoint (ValidToken tkn) options =
  req
    GET
    (https "api.freeagent.com" /: "v2" /: endpoint)
    NoReqBody
    jsonResponse
    ( oAuth2Bearer tkn
        <> header
          "User-Agent"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
        <> options
    )

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
    tkn <- getValidToken
    result <- embed $ E.try $ do
      r <- runReq defaultHttpConfig $ faRequest endpoint tkn options
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
