{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Token
  ( ValidToken (..),
    tokenFromAccessToken,
    tokenFromRefreshToken,
    runValidToken,
    runUseRefreshTokens,
    runSaveTokens,
    runGetAccessTokens,
    runGetTime,
    InvalidToken (..),
    runInvalidToken,
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

getAccessToken :: String -> String -> String -> IO (Tagged AccessToken TokenEndpoint)
getAccessToken cID secret authorizationCode = runReq defaultHttpConfig $ do
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

tokenFromAccessToken :: (Members '[Input (Tagged AccessToken TokenEndpoint), Output TokenEndpoint] r) => Sem (Input (Tagged AccessToken ValidToken) ': r) a -> Sem r a
tokenFromAccessToken = interpret $ \case
  Input -> do
    tokens <- unTagged <$> input @(Tagged AccessToken TokenEndpoint)
    output tokens
    return $ Tagged @AccessToken $ toValidToken tokens

tokenFromRefreshToken :: (Members '[Input (Tagged Refresh TokenEndpoint), Output TokenEndpoint] r) => Sem (Input (Tagged Refresh ValidToken) ': r) a -> Sem r a
tokenFromRefreshToken = interpret $ \case
  Input -> do
    tokens <- unTagged <$> input @(Tagged Refresh TokenEndpoint)
    output tokens
    return $ Tagged @Refresh $ toValidToken tokens

runValidToken :: (Members '[Input Config, Input UTCTime, Input (Tagged AccessToken ValidToken), Input (Tagged Refresh ValidToken), Output TokenEndpoint] r) => Sem (Input ValidToken ': r) a -> Sem r a
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

runUseRefreshTokens :: (Members '[Embed IO, Input Config, Trace, Output TokenEndpoint] r) => Sem (Input (Tagged Refresh TokenEndpoint) ': r) a -> Sem r a
runUseRefreshTokens = interpret $ \case
  Input -> do
    config <- input
    trace "Trying to refresh tokens"
    embed $ useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))

runGetTime :: (Members '[Embed IO] r) => Sem (Input UTCTime : r) a -> Sem r a
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runSaveTokens :: (Members '[Input UTCTime, Input Config, Output Config, Trace] r) => Sem (Output TokenEndpoint ': r) a -> Sem r a
runSaveTokens = interpret $ \case
  Output tokens -> do
    originalConfig <- input
    currentTime <- input
    output (withNewTokens tokens originalConfig currentTime)

runGetAccessTokens :: (Members '[Embed IO, Input Config, Trace, Output TokenEndpoint] r) => Sem (Input (Tagged AccessToken TokenEndpoint) ': r) a -> Sem r a
runGetAccessTokens = interpret $ \case
  Input -> do
    config <- input
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    embed $ getAccessToken (config ^. clientID) (config ^. clientSecret) authorizationCode

runInvalidToken :: (Members '[Input (Tagged Refresh ValidToken)] r) => Sem (Output InvalidToken ': r) a -> Sem r a
runInvalidToken = interpret $ \case
  Output _ -> do
    _ <- input @(Tagged Refresh ValidToken)
    return ()
