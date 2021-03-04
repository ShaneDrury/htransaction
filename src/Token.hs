{-# LANGUAGE TemplateHaskell #-}

module Token
  ( runGetTime,
    getAccessToken,
    runGetTokens,
    runAccessTokenM,
    exchangeAuthCode,
    exchangeRefreshToken,
    AccessTokenM (..),
    refreshAccessToken,
    getAuthCode,
    runWriteTokens,
    saveTokens,
  )
where

import Data.Maybe
import Data.Time
import Db
import Logger
import Polysemy
import Polysemy.BankAccount
import Polysemy.Input
import Polysemy.OAuth
import Polysemy.Output
import Polysemy.State
import Types
import Prelude

getAccessToken' :: TokenSet -> UTCTime -> Either InvalidTokenReason AccessToken
getAccessToken' TokenSet {..} currentTime =
  if tokenSetExpiresAt <= currentTime then Left Expired else Right tokenSetAccessToken

data AccessTokenM m a where
  GetAccessToken :: AccessTokenM m AccessToken
  RefreshAccessToken :: AccessTokenM m AccessToken

$(makeSem ''AccessTokenM)

tokenEndpoint2TokenSet :: TokenEndpoint -> UTCTime -> BankInstitution -> TokenSet
tokenEndpoint2TokenSet TokenEndpoint {..} currentTime institution =
  let expiresAt = addUTCTime (fromIntegral expires_in) currentTime
   in TokenSet
        { tokenSetAccessToken = access_token,
          tokenSetRefreshToken = refresh_token,
          tokenSetExpiresAt = expiresAt,
          tokenSetInstitution = institution
        }

runGetTime :: (Members '[Embed IO] r) => InterpreterFor (Input UTCTime) r
runGetTime = interpret $ \case
  Input -> embed getCurrentTime

runGetTokens :: (Members '[Logger, Input BankAccount, DbM] r) => InterpreterFor (Input (Maybe TokenSet)) r
runGetTokens = interpret $ \case
  Input -> do
    info "Loading tokens"
    institution <- getInstitution
    getTokensByInstitution institution

runAccessTokenM :: Members '[Input UTCTime, State (Maybe TokenSet), OAuthM] r => InterpreterFor AccessTokenM r
runAccessTokenM = interpret $ \case
  GetAccessToken -> do
    mtokenSet <- get @(Maybe TokenSet)
    currentTime <- input @UTCTime
    case mtokenSet of
      Nothing -> do
        authCode <- getAuthCode
        endpoint <- exchangeAuthCode authCode
        return $ access_token endpoint
      Just tokenSet ->
        case getAccessToken' tokenSet currentTime of
          Left Expired -> do
            endpoint <- exchangeRefreshToken (tokenSetRefreshToken tokenSet)
            return $ access_token endpoint
          Right tkn -> return tkn
  RefreshAccessToken -> do
    mtokenSet <- get @(Maybe TokenSet)
    case mtokenSet of
      Nothing -> do
        authCode <- getAuthCode
        endpoint <- exchangeAuthCode authCode
        return $ access_token endpoint
      Just tokenSet -> do
        endpoint <- exchangeRefreshToken (tokenSetRefreshToken tokenSet)
        return $ access_token endpoint

runWriteTokens :: (Members '[Logger, DbM, Input BankAccount] r) => InterpreterFor (Output (Maybe TokenSet)) r
runWriteTokens = interpret $ \case
  Output mtokens -> case mtokens of
    Nothing -> return ()
    Just tokens -> do
      info "Writing tokens"
      institution <- getInstitution
      updateTokens institution tokens

doSaveEndpoint :: (Members '[Input UTCTime, State (Maybe TokenSet), Input BankAccount] r) => TokenEndpoint -> Sem r ()
doSaveEndpoint endpoint = do
  currentTime <- input @UTCTime
  institution <- getInstitution
  put (Just $ tokenEndpoint2TokenSet endpoint currentTime institution)

saveTokens :: (Members '[Input UTCTime, State (Maybe TokenSet), OAuthM, Input BankAccount] r) => Sem r a -> Sem r a
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
