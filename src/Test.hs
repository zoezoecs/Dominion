{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, StandaloneDeriving #-}
module Test where

import Polysemy
import Polysemy.Input
import Base
import Types
import Effects
import Interpreters.Random
import System.Random.Stateful

data MyEffect m a where
  Wah :: Int -> MyEffect m Int
makeSem ''MyEffect

data MyLogEffect m a where
  LogWah :: Int -> MyLogEffect m ()
makeSem ''MyLogEffect

testIntercept :: IO Int
testIntercept = runM
     . interpret (\case Wah x -> embed . (>> return x) . putStrLn $ "real" ++ show x)
     . intercept (\case Wah x -> (embed . (>> return x) . putStrLn $ "intercepted3" ++ show x) >> wah x>> wah x)
     . intercept (\case Wah x -> wah x >> (embed . (>> return x) . putStrLn $ "Log Wah!:" ++ show x))
     . intercept (\case Wah x -> (embed . putStrLn $ "intercepted2" ++ show x) >> wah x >> wah x)
     . intercept (\case Wah x -> (embed . putStrLn $ "intercepted1" ++ show x) >> wah x >> wah x)
     $ wah 2

testLogAfter :: IO Int
testLogAfter = runM
              . interpret (\case (LogWah x) -> embed . putStrLn $ "log value:" ++ show x)
              . interpret (\case (Wah x) -> embed . (>> return x) . putStrLn $ "real" ++ show x)
              . intercept (\case (Wah x) -> wah x >>=/ logWah)
               $ wah 2 >> wah 3


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

-- TestStacks doesn't change the populated keys or the total set of card values
-- We want proofs that everything except CardFaces are in ActivePosition
-- The trouble is ActivePositions semantics depends on the interpretation, which
-- in turn depends on the value of the state it reads. That depends on Stacks not
-- changing any key validities, and the initialised value.
data TestStacks m a where
  ActivePositions :: TestStacks m [Position] -- TODO: abstraction barrier broken
  GetStack :: Position -> TestStacks m (Maybe [Card])
  ShuffleStack :: Position -> TestStacks m ()
  StackOnto :: Position -> Position -> TestStacks m ()
  DrawTo :: Position -> Position -> TestStacks m (Maybe Card)
  CardToPos :: Card -> Position -> TestStacks m ()
makeSem ''TestStacks
