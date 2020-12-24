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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Token
  ( runGetTime,
    TokenEndpoint (..),
    ValidTokenM (..),
    getValidToken,
    TokenM (..),
    invalidateTokens,
    ApiTokenM (..),
    getToken,
    getAccessToken,
    getAccessTokenNetwork,
    getRefreshToken,
    useRefreshToken,
    toValidToken,
    withNewTokens,
    runApiTokenMConst,
    clientID,
    clientSecret,
    token,
    tokenExpiresAt,
    refreshToken,
    configToken,
    Tokens (..),
    authorizationUrl,
    runGetTokens,
    runGetToken,
    TokensConfig (..),
  )
where

import Config (BankInstitution (..), bankInstitution)
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Maybe
import Data.Text
import Data.Time
import GHC.Generics
import Logger
import Network.HTTP.Req
import Polysemy
import Polysemy.BankAccount
import Polysemy.Input
import Polysemy.State
import Types
import Prelude

data Tokens
  = Tokens
      { _token :: Maybe String,
        _refreshToken :: Maybe String,
        _clientID :: String,
        _clientSecret :: String,
        _tokenExpiresAt :: Maybe UTCTime
      }
  deriving (Eq, Generic, Show)

newtype TokensConfig
  = TokensConfig (Map.Map BankInstitution Tokens)
  deriving stock (Eq, Generic, Show)

$(makeLenses ''Tokens)

$(makeLenses ''TokensConfig)

$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 1} ''Tokens)

$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 1} ''TokensConfig)

configToken :: Tokens -> UTCTime -> Either InvalidTokenReason ValidToken
configToken config currentTime =
  case config ^. token of
    Just t ->
      case config ^. tokenExpiresAt of
        Just expires ->
          if expires <= currentTime
            then Left Expired
            else Right $ ValidToken $ BS.pack t
        Nothing -> Left Missing
    Nothing -> Left Missing

data ValidTokenM m a where
  GetValidToken :: ValidTokenM m ValidToken
  InvalidateTokens :: ValidTokenM m ()

$(makeSem ''ValidTokenM)

data TokenM m a where
  GetToken :: TokenM m (Either InvalidTokenReason ValidToken)

$(makeSem ''TokenM)

runGetToken :: Members '[Input UTCTime, State Tokens] r => InterpreterFor TokenM r
runGetToken = interpret $ \case
  GetToken -> configToken <$> get @Tokens <*> input @UTCTime

data TokenEndpoint
  = TokenEndpoint
      { access_token :: String,
        token_type :: String,
        expires_in :: Integer,
        refresh_token :: Maybe String
      }
  deriving (Eq, Generic, Show, FromJSON)

data ApiTokenM m a where
  GetRefreshToken :: ApiTokenM m TokenEndpoint
  GetAccessToken :: ApiTokenM m TokenEndpoint

$(makeSem ''ApiTokenM)

withNewTokens :: TokenEndpoint -> Tokens -> UTCTime -> Tokens
withNewTokens TokenEndpoint {..} original currentTime =
  let expiresAt = Just $ addUTCTime (fromIntegral expires_in) currentTime
   in case refresh_token of
        Just _ ->
          original
            { _token = Just access_token,
              _refreshToken = refresh_token,
              _tokenExpiresAt = expiresAt
            }
        Nothing ->
          original
            { _token = Just access_token,
              _tokenExpiresAt = expiresAt
            }

userAgentHeader :: Option scheme
userAgentHeader = header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"

getAccessTokenNetwork :: String -> String -> String -> IO TokenEndpoint
getAccessTokenNetwork cID secret authorizationCode = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (https "api.freeagent.com" /: "v2" /: "token_endpoint")
      ( ReqBodyUrlEnc $
          "client_id" =: cID
            <> "client_secret" =: secret
            <> "code" =: authorizationCode
            <> "redirect_uri" =: ("https://developers.google.com/oauthplayground" :: Text)
            <> "scope" =: ("" :: Text)
            <> "grant_type" =: ("authorization_code" :: Text)
      )
      jsonResponse
      userAgentHeader
  let body = responseBody r :: TokenEndpoint
  return body

useRefreshToken :: String -> String -> String -> IO TokenEndpoint
useRefreshToken cID secret refresh = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (https "api.freeagent.com" /: "v2" /: "token_endpoint")
      ( ReqBodyUrlEnc $
          "client_id" =: cID
            <> "client_secret" =: secret
            <> "refresh_token" =: refresh
            <> "grant_type" =: ("refresh_token" :: Text)
      )
      jsonResponse
      userAgentHeader
  let body = responseBody r :: TokenEndpoint
  return body

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

toValidToken :: TokenEndpoint -> ValidToken
toValidToken endpoint = ValidToken $ BS.pack $ access_token endpoint

runGetTime :: (Members '[Embed IO] r) => InterpreterFor (Input UTCTime) r
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runApiTokenMConst :: TokenEndpoint -> TokenEndpoint -> InterpreterFor ApiTokenM r
runApiTokenMConst refresh access = interpret $ \case
  GetRefreshToken -> return refresh
  GetAccessToken -> return access

runGetTokens :: (Members '[Logger, BankAccountsM, Embed IO] r) => FilePath -> InterpreterFor (Input Tokens) r
runGetTokens fp = interpret $ \case
  Input -> do
    info $ "Loading tokens from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict @TokensConfig fp
    bankAccount <- getBankAccount
    case ecfg of
      Left e -> error e
      Right (TokensConfig cfg) -> case Map.lookup (bankAccount ^. bankInstitution) cfg of
        Just bankcfg -> return bankcfg
        Nothing -> error "Missing bank account config"
