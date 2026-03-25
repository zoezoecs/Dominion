module MyLib (main) where

import Polysemy
import Polysemy.State

import Base
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

main :: Members '[BoardInit, PlayerIO, Stacks, Log] r => [Player] -> [CardFace] -> Sem r ()
main pl cf =  evalState @GameState (initGS pl) .
             interpStateRead .
             interpCardEffects .
             interpStateWrite $ playGame pl cf

-- TODO: Log interception, reactions, add all cards.
