{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random
  ( RandomM (..),
    runRandomROnIO,
    randomString,
  )
where

import Control.Monad
import Data.Char
import Polysemy
import System.Random
import Prelude

data RandomM m a where
  RandomString :: Int -> RandomM m String

$(makeSem ''RandomM)

randomASCII :: IO Char
randomASCII = getStdRandom $ randomR (chr 97, chr 122)

runRandomROnIO :: (Members '[Embed IO] r) => InterpreterFor RandomM r
runRandomROnIO = interpret $ \case
  RandomString n -> replicateM n (embed randomASCII)
