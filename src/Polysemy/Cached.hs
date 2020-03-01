{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Cached
  ( runCached,
  )
where

import Control.Monad
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace
import Prelude

data Cached a = Cached a | Dirty deriving (Eq, Show)

runCached :: forall v a r. (Eq v) => Members '[Trace, Input v, Output v] r => Sem (State (Cached v) : r) a -> Sem r a
runCached =
  evalState Dirty
    . intercept @(Input v)
      ( \case
          Input -> do
            cached <- get @(Cached v)
            case cached of
              Cached cfg -> return cfg
              Dirty -> do
                v <- input
                put $ Cached v
                return v
      )
    . intercept @(Output v)
      ( \case
          Output v -> do
            cached <- get @(Cached v)
            case cached of
              Cached old ->
                when (old /= v) $ do
                  output v
                  put $ Cached v
              Dirty -> do
                output v
                put $ Cached v
      )
