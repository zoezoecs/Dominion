module MyLib (main) where

import Polysemy
import Polysemy.State
import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Data
import Interpreters
import Effects
import GameLoop

-- Missing tricky mechanics:
-- "First time"
-- Cost reduction
-- Overpay
-- Extra turns - Possession
-- Haggler, Talisman, Royal Seal changes each buy
-- Contraband/Embargo gives buying restrictions or penalties
-- Cavalry/Villa: Buy phase back to action

initGS :: [Player] -> GameState
initGS players = MkGameState {players = players,
  blocks = constMap players False,
  current_player = minimum players,
  current_actions = 0,
  current_buys = 0,
  current_currency = 0
  -- reactions :: [Reaction m]
}

initStacks :: [Player] -> [CardFace] -> Map Position [Card]
initStacks pl cf = wah1a `Map.union` wah3
  where 
    wah1a = Map.fromList $ map (\x -> (x,[])) $ liftA2 PlayerCard pl allPositions
    wah1b = [(PlayerCard p PlayerHand, [Estate, Estate, Estate]) | p <- pl]
    wah2 = Map.mapKeys Supply $ initialMap pl cf
    wah3 = Map.fromList [(Trash, [])]

main :: Members '[PlayerIO, Log] r => [Player] -> [CardFace] -> Sem r ()
main pl cf = evalState @(Map Position [Card]) (initStacks pl cf) .
             interpStacks .
             evalState @GameState (initGS pl) .
             interpStateRead .
             interpCardEffects .
             interpStateWrite $ playGame pl

-- TODO: Log interception, reactions, add all cards, IO.
