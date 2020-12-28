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
            cachedConfig <- get @(Cached v)
            case cachedConfig of
              Dirty -> do
                cfg <- input @v
                put @(Cached v) (Cached cfg)
                return cfg
              Cached cfg -> return cfg
          Put cfg -> do
            output @v cfg
            put @(Cached v) (Cached cfg)
      )
