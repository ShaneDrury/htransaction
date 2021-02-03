{-# LANGUAGE TemplateHaskell #-}

module Fa
  ( FaM (..),
    getFa,
    runFaM,
    FaTransaction (..),
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Http
import Types
import Prelude hiding (log)

data FaTransaction = FaTransaction
  { dated_on :: TransactionDate,
    description :: Text,
    amount :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data FaM v m a where
  GetFa :: Text -> Option 'Https -> FaM v m (Either ApiError v)

$(makeSem ''FaM)

runFaM ::
  forall v r.
  ( Members
      '[ ApiHttpM v
       ]
      r
  ) =>
  InterpreterFor (FaM v) r
runFaM = interpret $ \case
  GetFa endpoint options -> runApiRequest (https "api.freeagent.com" /: "v2" /: endpoint) GET NoReqBody options
