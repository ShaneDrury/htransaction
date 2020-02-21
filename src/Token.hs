{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Token where

import Config
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import Data.Maybe
import Data.Tagged
import Data.Text
import Data.Time
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import Types

newtype ValidToken = ValidToken BS.ByteString

data Refresh = Refresh

data AccessToken = AccessToken

data TokenEndpoint
  = TokenEndpoint
      { access_token :: String,
        token_type :: String,
        expires_in :: Integer,
        refresh_token :: Maybe String
      }
  deriving (Eq, Generic, Show, FromJSON)

withNewTokens :: TokenEndpoint -> Config -> IO Config
withNewTokens TokenEndpoint {..} original = do
  currentTime <- getCurrentTime
  case refresh_token of
    Just rt ->
      return $
        original
          { _token = Just access_token,
            _refreshToken = refresh_token,
            _tokenExpiresAt = Just $ addUTCTime (fromIntegral expires_in) currentTime
          }
    Nothing ->
      return $
        original
          { _token = Just access_token,
            _tokenExpiresAt = Just $ addUTCTime (fromIntegral expires_in) currentTime
          }

getAccessToken clientID clientSecret authorizationCode = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (https "api.freeagent.com" /: "v2" /: "token_endpoint")
      ( ReqBodyUrlEnc $
          "client_id" =: clientID
            <> "client_secret" =: clientSecret
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

useRefreshToken clientID clientSecret refreshToken = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (https "api.freeagent.com" /: "v2" /: "token_endpoint")
      ( ReqBodyUrlEnc $
          "client_id" =: clientID
            <> "client_secret" =: clientSecret
            <> "refresh_token" =: refreshToken
            <> "grant_type" =: ("refresh_token" :: Text)
      )
      jsonResponse
      ( header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
  let body = responseBody r :: TokenEndpoint
  return $ Tagged @Refresh body

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

runValidToken :: (Members '[Embed IO, Input Config, Input (Tagged AccessToken TokenEndpoint), Input (Tagged Refresh TokenEndpoint), Output TokenEndpoint] r) => Sem (Input ValidToken ': r) a -> Sem r a
runValidToken = interpret $ \case
  Input -> do
    config <- input
    case config ^. token of
      Just t -> do
        case config ^. tokenExpiresAt of
          Just expires -> do
            currentTime <- embed getCurrentTime
            if expires <= currentTime
              then refreshTokens
              else return $ ValidToken $ BS.pack t
          Nothing -> getSaveTokens
        return $ ValidToken $ BS.pack t
      Nothing -> getSaveTokens
    where
      getSaveTokens = do
        tokens <- input @(Tagged AccessToken TokenEndpoint)
        output $ unTagged tokens
        return $ ValidToken $ BS.pack $ access_token (unTagged tokens)
      refreshTokens = do
        tokens <- input @(Tagged Refresh TokenEndpoint)
        output $ unTagged tokens
        return $ ValidToken $ BS.pack $ access_token (unTagged tokens)

runSaveTokensStdout :: (Members '[Embed IO] r) => Sem (Output TokenEndpoint ': r) a -> Sem r a
runSaveTokensStdout = interpret $ \case
  Output tokens -> embed $ print tokens

runUseRefreshTokens :: (Members '[Embed IO, Input Config, Trace, Output TokenEndpoint] r) => Sem (Input (Tagged Refresh TokenEndpoint) ': r) a -> Sem r a
runUseRefreshTokens = interpret $ \case
  Input -> do
    config <- input
    trace "Trying to refresh tokens"
    useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))

runSaveTokens :: (Members '[Embed IO, Input Config, Trace] r) => FilePath -> Sem (Output TokenEndpoint ': r) a -> Sem r a
runSaveTokens fp = interpret $ \case
  Output tokens -> do
    originalConfig <- input
    newConfig <- embed $ withNewTokens tokens originalConfig
    embed $ S.writeFile fp (encode newConfig)

runGetAccessTokens :: (Members '[Embed IO, Input Config, Trace, Output TokenEndpoint] r) => Sem (Input (Tagged AccessToken TokenEndpoint) ': r) a -> Sem r a
runGetAccessTokens = interpret $ \case
  Input -> do
    config <- input
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    getAccessToken (config ^. clientID) (config ^. clientSecret) authorizationCode
