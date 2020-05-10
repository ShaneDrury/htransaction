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
  ( runValidToken,
    saveTokens,
    runGetTime,
    TokenEndpoint (..),
    runGetToken,
    ValidTokenM (..),
    getValidToken,
    TokenM,
    invalidateTokens,
    runApiTokenM,
    ApiTokenM,
    runApiTokenMConst,
  )
where

import Config
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Text
import Data.Time
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Config
import Polysemy.Input
import Types
import Prelude

data ValidTokenM m a where
  GetValidToken :: ValidTokenM m ValidToken
  InvalidateTokens :: ValidTokenM m ()

$(makeSem ''ValidTokenM)

data TokenM m a where
  GetToken :: TokenM m (Either InvalidToken ValidToken)

$(makeSem ''TokenM)

runGetToken :: Members '[Input UTCTime, ConfigM] r => InterpreterFor TokenM r
runGetToken = interpret $ \case
  GetToken -> configToken <$> getConfig <*> input @UTCTime

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

withNewTokens :: TokenEndpoint -> Config -> UTCTime -> Config
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
      ( header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
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
      ( header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
  let body = responseBody r :: TokenEndpoint
  return body

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

toValidToken :: TokenEndpoint -> ValidToken
toValidToken endpoint = ValidToken $ BS.pack $ access_token endpoint

runValidToken :: (Members '[ApiTokenM, TokenM, ConfigM, Input UTCTime] r) => InterpreterFor ValidTokenM r
runValidToken = interpret $ \case
  GetValidToken -> do
    eValidToken <- getToken
    case eValidToken of
      Left (InvalidToken Missing) -> toValidToken <$> getAccessToken
      Left (InvalidToken Expired) -> toValidToken <$> getRefreshToken
      Right (ValidToken validToken) -> return $ ValidToken validToken
  InvalidateTokens -> do
    oldCfg <- getConfig
    currentTime <- input
    writeConfig (oldCfg & tokenExpiresAt ?~ currentTime)

runGetTime :: (Members '[Embed IO] r) => InterpreterFor (Input UTCTime) r
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runApiTokenM :: (Members '[Embed IO, ConfigM, Logger] r) => InterpreterFor ApiTokenM r
runApiTokenM = interpret $ \case
  GetRefreshToken -> do
    config <- getConfig
    warn "Trying to refresh tokens"
    embed $ useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))
  GetAccessToken -> do
    config <- getConfig
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    embed $ getAccessTokenNetwork (config ^. clientID) (config ^. clientSecret) authorizationCode

doSaveTokens :: (Members '[Input UTCTime, ConfigM, ApiTokenM] r) => TokenEndpoint -> Sem r TokenEndpoint
doSaveTokens tokens = do
  originalConfig <- getConfig
  currentTime <- input @UTCTime
  writeConfig (withNewTokens tokens originalConfig currentTime)
  return tokens

saveTokens :: (Members '[Input UTCTime, ConfigM, ApiTokenM] r) => Sem r a -> Sem r a
saveTokens = intercept @ApiTokenM $ \case
  GetRefreshToken -> getRefreshToken >>= doSaveTokens
  GetAccessToken -> getAccessToken >>= doSaveTokens

runApiTokenMConst :: TokenEndpoint -> TokenEndpoint -> InterpreterFor ApiTokenM r
runApiTokenMConst refresh access = interpret $ \case
  GetRefreshToken -> return refresh
  GetAccessToken -> return access
