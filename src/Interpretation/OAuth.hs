module Interpretation.OAuth
  ( runApiTokenMConst,
    refreshTokenRequest,
    accessTokenRequest,
    runOAuthMOnFa,
    runOAuthMOnMonzo,
    runOAuthMOnInstitution,
  )
where

import Data.Text
import Db
import Fa
import Logger
import Monzo
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Http
import Polysemy.Input
import Polysemy.OAuth
import Polysemy.Random
import Types
import Prelude

runApiTokenMConst :: TokenEndpoint -> TokenEndpoint -> AuthorizationCode -> InterpreterFor OAuthM r
runApiTokenMConst refresh access authcode = interpret $ \case
  ExchangeRefreshToken _ -> return refresh
  ExchangeAuthCode _ -> return access
  GetAuthCode -> return authcode

refreshTokenRequest :: (Members '[HttpM TokenEndpoint] r) => Url 'Https -> Text -> Text -> RefreshToken -> Sem r (Either ApiError TokenEndpoint)
refreshTokenRequest endpoint cID secret (RefreshToken refresh) =
  runRequest
    endpoint
    POST
    ( ReqBodyUrlEnc $
        "client_id" =: cID
          <> "client_secret" =: secret
          <> "refresh_token" =: refresh
          <> "grant_type" =: ("refresh_token" :: Text)
    )
    mempty

accessTokenRequest :: (Members '[HttpM TokenEndpoint] r) => Url 'Https -> Text -> Text -> AuthorizationCode -> Sem r (Either ApiError TokenEndpoint)
accessTokenRequest endpoint cID secret (AuthorizationCode authorizationCode) =
  runRequest
    endpoint
    POST
    ( ReqBodyUrlEnc $
        "client_id" =: cID
          <> "client_secret" =: secret
          <> "code" =: authorizationCode
          <> "redirect_uri" =: ("https://developers.google.com/oauthplayground" :: Text)
          <> "scope" =: ("" :: Text)
          <> "grant_type" =: ("authorization_code" :: Text)
    )
    mempty

runOAuthMOnFa ::
  ( Members
      '[ Error ApiError,
         Logger,
         Input Client,
         HttpM TokenEndpoint,
         Embed IO
       ]
      r
  ) =>
  InterpreterFor OAuthM r
runOAuthMOnFa = interpret $ \case
  ExchangeAuthCode authCode -> do
    warn "Exchanging auth code"
    config <- input @Client
    r <- accessTokenRequest faAuthEndpoint (clientIdentifier config) (clientSecret config) authCode
    case r of
      Right rr -> return rr
      Left e -> throw e
  ExchangeRefreshToken tkn -> do
    warn "Trying to refresh tokens"
    config <- input @Client
    r <- refreshTokenRequest faAuthEndpoint (clientIdentifier config) (clientSecret config) tkn
    case r of
      Right rr -> return rr
      Left e -> throw e
  GetAuthCode -> do
    config <- input @Client
    embed $ putStrLn $ unpack $ "Open and copy code: " <> faAuthorizationUrl (clientIdentifier config)
    AuthorizationCode <$> embed getLine

runOAuthMOnMonzo ::
  ( Members
      '[ Error ApiError,
         Logger,
         Input Client,
         HttpM TokenEndpoint,
         Embed IO,
         RandomM
       ]
      r
  ) =>
  InterpreterFor OAuthM r
runOAuthMOnMonzo = interpret $ \case
  ExchangeAuthCode authCode -> do
    warn "Exchanging auth code"
    config <- input @Client
    r <- accessTokenRequest monzoAuthEndpoint (clientIdentifier config) (clientSecret config) authCode
    case r of
      Right rr -> return rr
      Left e -> throw e
  ExchangeRefreshToken tkn -> do
    warn "Trying to refresh tokens"
    config <- input @Client
    r <- refreshTokenRequest monzoAuthEndpoint (clientIdentifier config) (clientSecret config) tkn
    case r of
      Right rr -> return rr
      Left e -> throw e
  GetAuthCode -> do
    config <- input @Client
    state <- randomString 30
    embed $ putStrLn $ unpack $ "Open and copy code: " <> monzoAuthUrl (clientIdentifier config) state
    AuthorizationCode <$> embed getLine

runOAuthMOnInstitution :: (Members '[Input Client, Error ApiError, HttpM TokenEndpoint, Embed IO, Logger, RandomM] r) => BankInstitution -> InterpreterFor OAuthM r
runOAuthMOnInstitution institution = case institution of Fa -> runOAuthMOnFa; Monzo -> runOAuthMOnMonzo
