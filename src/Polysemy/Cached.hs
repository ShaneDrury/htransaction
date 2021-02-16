module Polysemy.Cached
  ( runStateCached,
    Cached (..),
  )
where

import Control.Monad
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Prelude

data Cached a = Cached a | Dirty
  deriving stock (Eq, Ord, Show, Functor)

runStateCached :: forall v r a. Sem (State v : r) a -> Sem (Output v : Input v : r) a
runStateCached =
  evalState @(Cached v) Dirty
    . reinterpret3
      ( \case
          Get -> do
            cached <- get @(Cached v)
            case cached of
              Dirty -> do
                newVal <- input @v
                put @(Cached v) (Cached newVal)
                return newVal
              Cached val -> return val
          Put val -> do
            output @v val
            put @(Cached v) (Cached val)
      )
