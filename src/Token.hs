{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Token where

import Config
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Tagged
import Data.Text
import Data.Time
import GHC.Generics
import Network.HTTP.Req

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
