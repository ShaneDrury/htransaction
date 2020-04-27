{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Token
  ( ValidToken (..),
    runValidToken,
    runUseRefreshTokens,
    runSaveRefreshTokens,
    runSaveAccessTokens,
    runGetAccessTokens,
    runGetTime,
    InvalidToken (..),
    Refresh,
    AccessToken,
    TokenEndpoint (..),
  )
where

import Config
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Tagged
import Data.Text
import Data.Time
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Config
import Polysemy.Input
import Polysemy.Output
import Types
import Prelude

newtype ValidToken = ValidToken BS.ByteString deriving stock (Eq, Show)

data InvalidToken = InvalidToken

data Refresh

data AccessToken

data TokenEndpoint
  = TokenEndpoint
      { access_token :: String,
        token_type :: String,
        expires_in :: Integer,
        refresh_token :: Maybe String
      }
  deriving (Eq, Generic, Show, FromJSON)

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

getAccessTokenNetwork :: String -> String -> String -> IO (Tagged AccessToken TokenEndpoint)
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
  return $ Tagged @AccessToken body

useRefreshToken :: String -> String -> String -> IO (Tagged Refresh TokenEndpoint)
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
  return $ Tagged @Refresh body

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

toValidToken :: Tagged b TokenEndpoint -> ValidToken
toValidToken tagged = ValidToken $ BS.pack $ access_token (unTagged tagged)

runValidToken :: (Members '[ConfigM, Input UTCTime, Input (Tagged AccessToken TokenEndpoint), Input (Tagged Refresh TokenEndpoint)] r) => Sem (Input ValidToken ': r) a -> Sem r a
runValidToken = interpret $ \case
  Input -> do
    config <- getConfig
    case config ^. token of
      Just t -> do
        newToken <- case config ^. tokenExpiresAt of
          Just expires -> do
            currentTime <- input
            if expires <= currentTime
              then toValidToken <$> input @(Tagged Refresh TokenEndpoint)
              else return $ ValidToken $ BS.pack t
          Nothing -> toValidToken <$> input @(Tagged AccessToken TokenEndpoint)
        return newToken
      Nothing -> toValidToken <$> input @(Tagged AccessToken TokenEndpoint)

runUseRefreshTokens :: (Members '[Embed IO, ConfigM, Logger] r) => Sem (Input (Tagged Refresh TokenEndpoint) ': r) a -> Sem r a
runUseRefreshTokens = interpret $ \case
  Input -> do
    config <- getConfig
    warn "Trying to refresh tokens"
    embed $ useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))

runGetTime :: (Members '[Embed IO] r) => Sem (Input UTCTime : r) a -> Sem r a
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runSaveRefreshTokens :: (Members '[Input UTCTime, ConfigM, Output Config, Input (Tagged Refresh TokenEndpoint)] r) => Sem r a -> Sem r a
runSaveRefreshTokens = intercept @(Input (Tagged Refresh TokenEndpoint)) $ \case
  Input -> do
    taggedTokens <- input @(Tagged Refresh TokenEndpoint)
    let tokens = unTagged taggedTokens
    originalConfig <- getConfig
    currentTime <- input
    output (withNewTokens tokens originalConfig currentTime)
    return taggedTokens

runSaveAccessTokens :: (Members '[Input UTCTime, ConfigM, Output Config, Input (Tagged AccessToken TokenEndpoint)] r) => Sem r a -> Sem r a
runSaveAccessTokens = intercept @(Input (Tagged AccessToken TokenEndpoint)) $ \case
  Input -> do
    taggedTokens <- input @(Tagged AccessToken TokenEndpoint)
    let tokens = unTagged taggedTokens
    originalConfig <- getConfig
    currentTime <- input
    output (withNewTokens tokens originalConfig currentTime)
    return taggedTokens

runGetAccessTokens :: (Members '[Embed IO, ConfigM] r) => Sem (Input (Tagged AccessToken TokenEndpoint) ': r) a -> Sem r a
runGetAccessTokens = interpret $ \case
  Input -> do
    config <- getConfig
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    embed $ getAccessTokenNetwork (config ^. clientID) (config ^. clientSecret) authorizationCode
