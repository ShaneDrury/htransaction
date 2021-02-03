{-# LANGUAGE TemplateHaskell #-}

module Token
  ( runGetTime,
    TokenEndpoint (..),
    OAuthM (..),
    getAccessToken,
    runApiTokenMConst,
    clientID,
    clientSecret,
    accessToken,
    tokenExpiresAt,
    refreshToken,
    TokenSet (..),
    authorizationUrl,
    runGetTokens,
    runAccessTokenM,
    BankInstitutionTokens (..),
    monzoAuthUrl,
    exchangeAuthCode,
    exchangeRefreshToken,
    updateTokens,
    AccessTokenM (..),
    AccessToken (..),
    refreshAccessToken,
    getAuthCode,
    AuthorizationCode (..),
    RefreshToken (..),
  )
where

import Config (BankInstitution (..))
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Maybe
import Data.Text
import Data.Time
import GHC.Generics
import Logger
import Polysemy
import Polysemy.BankAccount
import Polysemy.Input
import Polysemy.State
import Types
import Prelude

newtype AccessToken = AccessToken Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype RefreshToken = RefreshToken Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AuthorizationCode = AuthorizationCode String deriving stock (Eq, Show)

data TokenSet = TokenSet
  { _accessToken :: Maybe AccessToken,
    _refreshToken :: Maybe RefreshToken,
    _clientID :: String,
    _clientSecret :: String,
    _tokenExpiresAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)

newtype BankInstitutionTokens
  = BankInstitutionTokens (Map.Map BankInstitution TokenSet)
  deriving stock (Eq, Show, Generic)

$(makeLenses ''TokenSet)

$(makeLenses ''BankInstitutionTokens)

$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 1} ''TokenSet)

$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 1} ''BankInstitutionTokens)

getAccessToken' :: TokenSet -> UTCTime -> Either InvalidTokenReason AccessToken
getAccessToken' tokenSet currentTime =
  case tokenSet ^. accessToken of
    Just t ->
      case tokenSet ^. tokenExpiresAt of
        Just expires ->
          if expires <= currentTime
            then Left Expired
            else Right t
        Nothing -> Left Missing
    Nothing -> Left Missing

data AccessTokenM m a where
  GetAccessToken :: AccessTokenM m AccessToken
  RefreshAccessToken :: AccessTokenM m AccessToken

$(makeSem ''AccessTokenM)

data TokenEndpoint = TokenEndpoint
  { access_token :: AccessToken,
    token_type :: String,
    expires_in :: Integer,
    refresh_token :: RefreshToken,
    refresh_token_expires_in :: Maybe Integer
  }
  deriving stock (Eq, Show)
  deriving anyclass (FromJSON)
  deriving stock (Generic)

data OAuthM m a where
  GetAuthCode :: OAuthM m AuthorizationCode
  ExchangeAuthCode :: AuthorizationCode -> OAuthM m TokenEndpoint
  ExchangeRefreshToken :: RefreshToken -> OAuthM m TokenEndpoint

$(makeSem ''OAuthM)

updateTokens :: TokenEndpoint -> TokenSet -> UTCTime -> TokenSet
updateTokens TokenEndpoint {..} original currentTime =
  let expiresAt = Just $ addUTCTime (fromIntegral expires_in) currentTime
   in original
        { _accessToken = Just access_token,
          _refreshToken = Just refresh_token,
          _tokenExpiresAt = expiresAt
        }

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

monzoAuthUrl :: String -> String -> String
monzoAuthUrl clientId state =
  "https://auth.monzo.com/?client_id=" <> clientId <> "&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground&response_type=code&state=" <> state

runGetTime :: (Members '[Embed IO] r) => InterpreterFor (Input UTCTime) r
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runApiTokenMConst :: TokenEndpoint -> TokenEndpoint -> AuthorizationCode -> InterpreterFor OAuthM r
runApiTokenMConst refresh access authcode = interpret $ \case
  ExchangeRefreshToken _ -> return refresh
  ExchangeAuthCode _ -> return access
  GetAuthCode -> return authcode

runGetTokens :: (Members '[Logger, BankAccountsM, Embed IO] r) => FilePath -> InterpreterFor (Input TokenSet) r
runGetTokens fp = interpret $ \case
  Input -> do
    info $ "Loading tokens from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict @BankInstitutionTokens fp
    institution <- getInstitution
    case ecfg of
      Left e -> error e
      Right (BankInstitutionTokens cfg) -> case Map.lookup institution cfg of
        Just bankcfg -> return bankcfg
        Nothing -> error "Missing bank account config"

runAccessTokenM :: Members '[Input UTCTime, State TokenSet, OAuthM] r => InterpreterFor AccessTokenM r
runAccessTokenM = interpret $ \case
  GetAccessToken -> do
    tokenSet <- get @TokenSet
    currentTime <- input @UTCTime
    case getAccessToken' tokenSet currentTime of
      Left Expired -> refreshAccessToken_
      Left Missing -> do
        authCode <- getAuthCode
        endpoint <- exchangeAuthCode authCode
        return $ access_token endpoint
      Right tkn -> return tkn
  RefreshAccessToken -> refreshAccessToken_

refreshAccessToken_ :: Members '[State TokenSet, OAuthM] r => Sem r AccessToken
refreshAccessToken_ = do
  rtkn <- getRefreshToken_
  endpoint <- exchangeRefreshToken rtkn
  return $ access_token endpoint

getRefreshToken_ :: Members '[State TokenSet, OAuthM] r => Sem r RefreshToken
getRefreshToken_ = do
  tokenSet <- get @TokenSet
  case tokenSet ^. refreshToken of
    Nothing -> do
      authCode <- getAuthCode
      endpoint <- exchangeAuthCode authCode
      return $ refresh_token endpoint
    Just tkn -> return tkn
