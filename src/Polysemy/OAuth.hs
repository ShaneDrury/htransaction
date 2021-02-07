{-# LANGUAGE TemplateHaskell #-}

module Polysemy.OAuth
  ( OAuthM (..),
    getAuthCode,
    exchangeAuthCode,
    exchangeRefreshToken,
  )
where

import Polysemy
import Types
import Prelude ()

data OAuthM m a where
  GetAuthCode :: OAuthM m AuthorizationCode
  ExchangeAuthCode :: AuthorizationCode -> OAuthM m TokenEndpoint
  ExchangeRefreshToken :: RefreshToken -> OAuthM m TokenEndpoint

$(makeSem ''OAuthM)
