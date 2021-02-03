{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Http (HttpM (..), runRequest, runHttpMOnReq, ApiHttpM (..), runApiRequest, runApiHttpMOnTokens, retryOnUnauthorized) where

import qualified Control.Exception as E
import Control.Monad
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)
import Logger
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Request
import Token
import Types
import Prelude

data HttpM r m a where
  RunRequest :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpBody body) => Url 'Https -> method -> body -> Option 'Https -> HttpM v m (Either ApiError v)

$(makeSem ''HttpM)

data ApiHttpM r m a where
  RunApiRequest :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpBody body) => Url 'Https -> method -> body -> Option 'Https -> ApiHttpM v m (Either ApiError v)

$(makeSem ''ApiHttpM)

mkRequest :: (MonadHttp m, FromJSON a, HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpBody body) => Url 'Https -> method -> body -> Option 'Https -> m (JsonResponse a)
mkRequest url method body options =
  req
    method
    url
    body
    jsonResponse
    ( header
        "User-Agent"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
        <> options
    )

runHttpMOnReq :: forall v r. (Members '[Embed IO, Error HttpException] r, FromJSON v) => InterpreterFor (HttpM v) r
runHttpMOnReq = interpret $ \case
  RunRequest url method body options -> do
    result <- embed $
      E.try $ do
        r <- runReq defaultHttpConfig $ mkRequest url method body options
        return (responseBody r :: v)
    case result of
      Right res -> return $ Right (res :: v)
      Left err' ->
        if isUnauthorized err'
          then return $ Left Unauthorized
          else throw @HttpException err'

runApiHttpMOnTokens :: forall v r. (Members '[AccessTokenM, HttpM v] r) => InterpreterFor (ApiHttpM v) r
runApiHttpMOnTokens = interpret $ \case
  RunApiRequest url method body options -> do
    tkn <- getAccessToken
    runRequest url method body (tokenToHeader tkn <> options)

retryOnUnauthorized ::
  forall v r a.
  ( Members
      '[ Logger,
         ApiHttpM v,
         HttpM v,
         AccessTokenM
       ]
      r
  ) =>
  Sem r a ->
  Sem r a
retryOnUnauthorized =
  intercept @(ApiHttpM v)
    ( \case
        RunApiRequest url method body options -> do
          r <- runApiRequest @v url method body options
          case r of
            Left Unauthorized -> do
              err "Unauthorized"
              tkn <- refreshAccessToken
              runRequest @v url method body (tokenToHeader tkn <> options)
            s -> return s
    )

encodeToken :: AccessToken -> BS.ByteString
encodeToken (AccessToken tkn) = encodeUtf8 tkn

tokenToHeader :: AccessToken -> Option 'Https
tokenToHeader = oAuth2Bearer . encodeToken
