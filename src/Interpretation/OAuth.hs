module Interpretation.OAuth
  ( runApiTokenMConst,
    refreshTokenRequest,
    accessTokenRequest,
    runOAuthMOnFa,
    runOAuthMOnMonzo,
    runOAuthMOnInstitution,
  )
where

import Config
import Control.Lens
import Data.Text
import Fa
import Logger
import Monzo
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Http
import Polysemy.OAuth
import Polysemy.Random
import Polysemy.State
import Token
import Types
import Prelude

runApiTokenMConst :: TokenEndpoint -> TokenEndpoint -> AuthorizationCode -> InterpreterFor OAuthM r
runApiTokenMConst refresh access authcode = interpret $ \case
  ExchangeRefreshToken _ -> return refresh
  ExchangeAuthCode _ -> return access
  GetAuthCode -> return authcode

refreshTokenRequest :: (Members '[HttpM TokenEndpoint] r) => Url 'Https -> String -> String -> RefreshToken -> Sem r (Either ApiError TokenEndpoint)
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

accessTokenRequest :: (Members '[HttpM TokenEndpoint] r) => Url 'Https -> String -> String -> AuthorizationCode -> Sem r (Either ApiError TokenEndpoint)
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
         State TokenSet,
         HttpM TokenEndpoint,
         Embed IO
       ]
      r
  ) =>
  InterpreterFor OAuthM r
runOAuthMOnFa = interpret $ \case
  ExchangeAuthCode authCode -> do
    warn "Exchanging auth code"
    config <- get @TokenSet
    r <- accessTokenRequest faAuthEndpoint (config ^. clientID) (config ^. clientSecret) authCode
    case r of
      Right rr -> return rr
      Left e -> throw e
  ExchangeRefreshToken tkn -> do
    warn "Trying to refresh tokens"
    config <- get @TokenSet
    r <- refreshTokenRequest faAuthEndpoint (config ^. clientID) (config ^. clientSecret) tkn
    case r of
      Right rr -> return rr
      Left e -> throw e
  GetAuthCode -> do
    config <- get @TokenSet
    embed $ putStrLn $ "Open and copy code: " <> faAuthorizationUrl (config ^. clientID)
    AuthorizationCode <$> embed getLine

runOAuthMOnMonzo ::
  ( Members
      '[ Error ApiError,
         Logger,
         State TokenSet,
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
    config <- get @TokenSet
    r <- accessTokenRequest monzoAuthEndpoint (config ^. clientID) (config ^. clientSecret) authCode
    case r of
      Right rr -> return rr
      Left e -> throw e
  ExchangeRefreshToken tkn -> do
    warn "Trying to refresh tokens"
    config <- get @TokenSet
    r <- refreshTokenRequest monzoAuthEndpoint (config ^. clientID) (config ^. clientSecret) tkn
    case r of
      Right rr -> return rr
      Left e -> throw e
  GetAuthCode -> do
    config <- get @TokenSet
    state <- randomString 30
    embed $ putStrLn $ "Open and copy code: " <> monzoAuthUrl (config ^. clientID) state
    AuthorizationCode <$> embed getLine

runOAuthMOnInstitution :: (Members '[State TokenSet, Error ApiError, HttpM TokenEndpoint, Embed IO, Logger, RandomM] r) => BankInstitution -> InterpreterFor OAuthM r
runOAuthMOnInstitution institution = case institution of Fa -> runOAuthMOnFa; Monzo -> runOAuthMOnMonzo
