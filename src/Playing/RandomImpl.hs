{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Playing.RandomImpl where

import Polysemy
import Polysemy.Input
import Base
import Types
import Effects
import Interpreters.Random
import System.Random.Stateful

testRandomWithSeed :: IO ()
testRandomWithSeed = runM
                     . interpRandomWithSeed 3
                     $ testProgram

testRandom :: IO ()
testRandom = runM
                . interpRandomGlobal
                $ testProgram

testProgram :: Members '[Embed IO, RandomGenEff IO] r => Sem r ()
testProgram = testProgram' >> testProgram'

testProgram' :: Members '[Embed IO, RandomGenEff IO] r => Sem r ()
testProgram' = do
                HoldRandom gen <- input @(HoldRandom IO)
                word <- embed @IO $ uniformWord32 gen
                embed $ print word
