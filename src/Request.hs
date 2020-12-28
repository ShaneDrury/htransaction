{-# LANGUAGE TemplateHaskell #-}

module Request
  ( isUnauthorized,
  )
where

import Control.Lens
import qualified Network.HTTP.Client as H
import Network.HTTP.Req
import qualified Network.HTTP.Types.Status as Status
import Prelude hiding (log)

$(makePrisms ''HttpException) -- req

$(makePrisms ''H.HttpException)

$(makePrisms ''H.HttpExceptionContent)

responseP :: Traversal' HttpException (H.Response ())
responseP = _VanillaHttpException . _HttpExceptionRequest . _2 . _StatusCodeException . _1

statusP :: HttpException -> Maybe Status.Status
statusP e = H.responseStatus <$> e ^? responseP

isUnauthorized :: HttpException -> Bool
isUnauthorized e = case statusP e of
  Just status -> status == Status.unauthorized401
  Nothing -> False
