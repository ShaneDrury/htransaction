{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
    tokenFromTagged,
    runValidToken,
    runUseRefreshTokens,
    runSaveTokens,
    runGetAccessTokens,
    runGetTime,
    InvalidToken (..),
    Refresh,
    AccessToken,
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
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import Prelude

newtype ValidToken = ValidToken BS.ByteString

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

toValidToken :: TokenEndpoint -> ValidToken
toValidToken tokens = ValidToken $ BS.pack $ access_token tokens

tokenFromTagged :: forall b r a. (Members '[Input (Tagged b TokenEndpoint)] r) => Sem (Input (Tagged b ValidToken) ': r) a -> Sem r a
tokenFromTagged = interpret $ \case
  Input -> do
    tokens <- unTagged <$> input @(Tagged b TokenEndpoint)
    return $ Tagged @b $ toValidToken tokens

runValidToken :: (Members '[Input Config, Input UTCTime, Input (Tagged AccessToken ValidToken), Input (Tagged Refresh ValidToken)] r) => Sem (Input ValidToken ': r) a -> Sem r a
runValidToken = interpret $ \case
  Input -> do
    config <- input
    case config ^. token of
      Just t -> do
        _ <- case config ^. tokenExpiresAt of
          Just expires -> do
            currentTime <- input
            if expires <= currentTime
              then unTagged <$> input @(Tagged Refresh ValidToken)
              else return $ ValidToken $ BS.pack t
          Nothing -> unTagged <$> input @(Tagged AccessToken ValidToken)
        return $ ValidToken $ BS.pack t
      Nothing -> unTagged <$> input @(Tagged AccessToken ValidToken)

runUseRefreshTokens :: (Members '[Embed IO, Input Config, Trace] r) => Sem (Input (Tagged Refresh TokenEndpoint) ': r) a -> Sem r a
runUseRefreshTokens = interpret $ \case
  Input -> do
    config <- input
    trace "Trying to refresh tokens"
    embed $ useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))

runGetTime :: (Members '[Embed IO] r) => Sem (Input UTCTime : r) a -> Sem r a
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runSaveTokens :: forall b r a. (Members '[Input UTCTime, Input Config, Output Config, Trace] r) => Sem (Input (Tagged b TokenEndpoint) : r) a -> Sem (Input (Tagged b TokenEndpoint) : r) a
runSaveTokens = intercept @(Input (Tagged b TokenEndpoint)) $ \case
  Input -> do
    taggedTokens <- input @(Tagged b TokenEndpoint)
    let tokens = unTagged taggedTokens
    originalConfig <- input
    currentTime <- input
    output (withNewTokens tokens originalConfig currentTime)
    return taggedTokens

runGetAccessTokens :: (Members '[Embed IO, Input Config, Trace] r) => Sem (Input (Tagged AccessToken TokenEndpoint) ': r) a -> Sem r a
runGetAccessTokens = interpret $ \case
  Input -> do
    config <- input
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    embed $ getAccessTokenNetwork (config ^. clientID) (config ^. clientSecret) authorizationCode
