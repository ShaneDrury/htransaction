{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Random where

import Control.Monad
import Data.Char
import Polysemy
import System.Random

data RandomM m a where
  RandomString :: Int -> RandomM m String

$(makeSem ''RandomM)

randomASCII :: IO Char
randomASCII = getStdRandom $ randomR (chr 0, chr 127)

runRandomROnIO :: (Members '[Embed IO] r) => InterpreterFor RandomM r
runRandomROnIO = interpret $ \case
  RandomString n -> replicateM n (embed randomASCII)
