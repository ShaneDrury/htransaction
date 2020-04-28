{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Cached
  ( runCached,
    Cached (..),
  )
where

import Control.Monad
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Prelude

data Cached a = Cached a | Dirty deriving (Eq, Show)

runCached :: forall v r. (Eq v, Members '[Input v, Output v] r) => InterpreterFor (State (Cached v)) r
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
