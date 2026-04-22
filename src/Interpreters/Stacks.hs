{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Interpreters.Stacks where

import Polysemy
import Polysemy.State

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Types
import Effects


-- TODO: Fix maybes everywhere
-- TODO: Surely this is a lens type problem. At least modify??
unsafeLookup :: Ord k => k -> Map k c -> c
unsafeLookup l = fromJust . Map.lookup l

refill :: (Member Stacks r, Foldable t) => PileConfig t -> Position -> Sem r ()
refill (PileConfig fillFrom shuffleOnFill) l = trace "wah" $ do
    case Map.lookup l fillFrom of
      Nothing -> return ()
      Just l' -> do
        stackOnto l' l
        when (l `elem` shuffleOnFill) $ shuffleStack l

-- TODO: can i make this use a more efficient list esque representation?
interpStacks :: (Foldable t, Members '[State (Map Position [Card]), RandomShuffle] r) => PileConfig t -> Sem (Stacks : r) a -> Sem r a
interpStacks cfg = interpret $ \case
      ActivePositions -> Map.keys <$> get @(Map Position [Card])
      GetStack loc -> Map.lookup loc <$> get @(Map Position [Card])
      ShuffleStack loc -> do
        cardMap <- get @(Map Position [Card])
        let stack = unsafeLookup loc cardMap
        shuffled <- randomShuffle stack
        put $ Map.insert loc shuffled cardMap
      StackOnto l1 l2 -> do
        cardMap <- get @(Map Position [Card])
        let stack1 = unsafeLookup l1 cardMap
        let stack2 = unsafeLookup l2 cardMap
        put $ Map.insert l1 [] . Map.insert l2 (stack1++stack2) $ cardMap
      DrawTo l1 l2 -> do
        cardMap' <- get @(Map Position [Card])
        let stack1' = unsafeLookup l1 cardMap'
        when (null stack1') $ interpStacks cfg $ refill cfg l1
        cardMap <- get @(Map Position [Card])
        let stack1 = unsafeLookup l1 cardMap
        let stack2 = unsafeLookup l2 cardMap
        case stack1 of
            []                -> return Nothing
            (x:xs)            -> do
                put $ Map.insert l1 xs . Map.insert l2 (x:stack2) $ cardMap
                return $ Just x
      CardToPos card loc -> do
        cardMap <- get @(Map Position [Card])
        let notACard = Map.null $ Map.filter (elem card) cardMap
        when notACard undefined
        let newMap = fmap (filter (card /=)) cardMap -- undefined if this doesn't change it?
        let stack1 = unsafeLookup loc newMap
        void $ put $ Map.insert loc (card:stack1) newMap

