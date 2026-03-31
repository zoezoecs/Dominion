module Interpreters.Random where
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

import Polysemy
import Polysemy.Input

import Control.Monad

import System.Random.Shuffle
import System.Random
import System.Random.Stateful

import GHC.Conc

import Effects
myShuffle :: StatefulGen g m => g -> [a] -> m [a]
myShuffle gen xs = shuffle xs <$> replicateM (length xs - 1) (uniformRM (0, length xs - 1) gen)

interpRandomShuffle :: Members '[RandomGenEff IO, Embed IO] r => Sem (RandomShuffle : r) a -> Sem r a
interpRandomShuffle = interpret $ \case
  RandomShuffle stack -> do
    HoldRandom gen <- input @(HoldRandom IO)
    embed @IO $ myShuffle gen stack

-- newSTGenM :: g -> ST s (STGenM g s) 
-- MonadIO m => g -> m (AtomicGenM g) 
runInputGenerator :: (Member (Embed m) r) => (g2 -> m (g1 g2)) -> (k -> g2) -> k -> Sem (Input (g1 g2) ': r) a -> Sem r a
runInputGenerator makeGeneral makeBasic n program = do
  gen <- embed $ makeGeneral (makeBasic n)
  runInputConst gen program

runAtomicGenM :: Member (Embed IO) r => Int -> Sem (Input (AtomicGenM StdGen) ': r) a -> Sem r a
runAtomicGenM = runInputGenerator @IO newAtomicGenM mkStdGen

runIOGenM :: Member (Embed IO) r => Int -> Sem (Input (IOGenM StdGen) ': r) a -> Sem r a
runIOGenM = runInputGenerator @IO newIOGenM mkStdGen

runTGenM :: Member (Embed STM) r => Int -> Sem (Input (TGenM StdGen) ': r) a -> Sem r a
runTGenM = runInputGenerator @STM newTGenM mkStdGen

mapInput :: (i1 -> i0) -> Sem (Input i0 : r) a -> Sem (Input i1 : r) a
mapInput f = reinterpret $ \case Input -> f <$> input

constraintIn :: StatefulGen g IO => Sem (Input (HoldRandom IO) : r) a -> Sem (Input g : r) a
constraintIn = mapInput HoldRandom

monomorphiseAtomicGenM :: Sem (Input (HoldRandom IO) : r) a -> Sem (Input (AtomicGenM StdGen) : r) a
monomorphiseAtomicGenM = constraintIn

interpRandomWithSeed :: Members '[Embed IO] r => Int -> Sem (RandomGenEff IO : r) a -> Sem r a
interpRandomWithSeed n = runAtomicGenM n . constraintIn

interpRandomGlobal :: Member (Embed IO) r => Sem (RandomGenEff IO: r) a -> Sem r a
interpRandomGlobal = interpret $ \case
  Input -> return $ HoldRandom globalStdGen
