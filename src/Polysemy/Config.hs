module Polysemy.Config
  ( runGetConfig,
    runWriteConfig,
    runWriteTokens,
    saveTokens,
    runOAuthM,
  )
where

import Config
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map
import Data.Text
import Data.Time
import Logger
import Network.HTTP.Req
import Polysemy
import Polysemy.BankAccount
import Polysemy.Error
import Polysemy.Http
import Polysemy.Input
import Polysemy.Output
import Polysemy.Random
import Polysemy.State
import Token
import Types
import Prelude

runGetConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Input Config) r
runGetConfig fp = interpret $ \case
  Input -> do
    info $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runWriteTokens :: (Members '[Logger, Embed IO, BankAccountsM] r) => FilePath -> InterpreterFor (Output TokenSet) r
runWriteTokens fp = interpret $ \case
  Output cfg -> do
    info $ "Writing tokens to " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict @BankInstitutionTokens fp
    institution <- getInstitution
    case ecfg of
      Left e -> error e
      Right (BankInstitutionTokens tokensCfg) -> embed $ S.writeFile fp (encodePretty @BankInstitutionTokens $ BankInstitutionTokens $ Map.insert institution cfg tokensCfg)

runWriteConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Output Config) r
runWriteConfig fp = interpret $ \case
  Output cfg -> do
    info $ "Writing config to " ++ fp
    embed $ S.writeFile fp (encodePretty cfg)

institutionEndpoint :: BankInstitution -> Url 'Https
institutionEndpoint institution = case institution of
  Fa -> https "api.freeagent.com" /: "v2" /: "token_endpoint"
  Monzo -> https "api.monzo.com" /: "oauth2" /: "token"

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

runOAuthM :: (Members '[Embed IO, State TokenSet, Logger, BankAccountsM, RandomM, Error ApiError, HttpM TokenEndpoint] r) => InterpreterFor OAuthM r
runOAuthM = interpret $ \case
  GetAuthCode -> do
    config <- get @TokenSet
    institution <- getInstitution
    case institution of
      Fa -> embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
      Monzo -> do
        state <- randomString 30
        embed $ putStrLn $ "Open and copy code: " <> monzoAuthUrl (config ^. clientID) state
    AuthorizationCode <$> embed getLine
  ExchangeAuthCode authCode -> do
    config <- get @TokenSet
    institution <- getInstitution
    warn "Exchanging auth code"
    r <- accessTokenRequest (institutionEndpoint institution) (config ^. clientID) (config ^. clientSecret) authCode
    case r of
      Right rr -> return rr
      Left e -> throw e
  ExchangeRefreshToken tkn -> do
    config <- get @TokenSet
    institution <- getInstitution
    warn "Trying to refresh tokens"
    r <- refreshTokenRequest (institutionEndpoint institution) (config ^. clientID) (config ^. clientSecret) tkn
    case r of
      Right rr -> return rr
      Left e -> throw e

doSaveEndpoint :: (Members '[Input UTCTime, State TokenSet] r) => TokenEndpoint -> Sem r ()
doSaveEndpoint endpoint = do
  originalConfig <- get @TokenSet
  currentTime <- input @UTCTime
  put (updateTokens endpoint originalConfig currentTime)

saveTokens :: (Members '[Input UTCTime, State TokenSet, OAuthM] r) => Sem r a -> Sem r a
saveTokens = intercept @OAuthM $ \case
  GetAuthCode -> getAuthCode
  ExchangeAuthCode authCode -> do
    endpoint <- exchangeAuthCode authCode
    doSaveEndpoint endpoint
    return endpoint
  ExchangeRefreshToken tkn -> do
    endpoint <- exchangeRefreshToken tkn
    doSaveEndpoint endpoint
    return endpoint
