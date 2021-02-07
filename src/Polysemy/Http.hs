{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Http
  ( HttpM (..),
    runRequest,
    ApiHttpM (..),
    runApiRequest,
  )
where

import Network.HTTP.Req
import Polysemy
import Types
import Prelude

data HttpM r m a where
  RunRequest :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpBody body) => Url 'Https -> method -> body -> Option 'Https -> HttpM v m (Either ApiError v)

$(makeSem ''HttpM)

data ApiHttpM r m a where
  RunApiRequest :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpBody body) => Url 'Https -> method -> body -> Option 'Https -> ApiHttpM v m (Either ApiError v)

$(makeSem ''ApiHttpM)
