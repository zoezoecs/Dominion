module MyLib (wah) where

import Polysemy
import Polysemy.State

import Base
import Interpreters
import Effects
import GameLoop

-- class CardStack f where
--   empty :: f a
--   create :: [a] -> f a
--   destroy :: f a -> [a]
--   insert :: Int -> a -> f a -> f a
--   remove :: Int -> f a -> (Maybe a, f a)
-- 
-- newtype CardStackDefault a = CardStackDefault {getStack :: [a]}
-- 
-- instance CardStack CardStackDefault where
--   empty = CardStackDefault []
--   create = CardStackDefault
--   destroy = getStack
--   insert n a (CardStackDefault cs) = let (xs, ys) = splitAt n cs in CardStackDefault (xs ++ [a] ++ ys)
--   remove n (CardStackDefault cs) = case splitAt n cs of
--     (xs, []) -> (Nothing, CardStackDefault cs)
--     (xs, y:ys) -> (Just y, CardStackDefault $ xs ++ ys)
-- 
-- data AddCard postype cardtype n = AddCard postype cardtype n
-- data RemoveCard postype cardtype n = RemoveCard postype cardtype n
-- data MoveCard from n to m card = Move (AddCard to card n) (RemoveCard from card m)
-- 
-- data CardState ids = None


-- Card state interface which only allows physical manipulations of cards. It will allow for full inspection though.
-- Instance of this which implements dominion specific rules about drawing from empty deck
-- Implemented via some combinators?
--
-- Interface: you can initialise cards into various positions and then move them between positions. You can query for a cards location and query a position. You can draw a specific card out and place it back where it came from, or randomly in a deck.
-- How do we specify how to move them? How do we handle information?
-- Use random number generator, with drawing from top and bottom representing dirac generator?
-- Cards can be in ordered piles or unordered collections

-- Design choice: all cards have ids and aren't just handled as cards.

-- type CardSemantics = forall r. Members [BoardStateRead, CardEffects, PlayerIO] r => Player -> Card -> Sem r ()

-- Design choice: Effects that can be easily written as a composition are still separate effects that are just reinterpreted with the composition?
-- Design choice: Messages to clients entirely through separate messages, and logs are reinterpreted from effects
-- Partial information managed by different messages with less information, no state tracking, no ability to refer to
-- previous messages for granular information (the card that was drawn 2 turns ago was a ...)
-- Clients don't reconstruct state, they just display the required information and collect the moves.
-- Clients don't see the card logic causality, they just see streams of events and must infer themselves.
-- How to make sure enough information gets through? We need a protocol.

-- Design choice: Reaction effects checked once per type of trigger, at the interpretation of the triggerable event.
-- Since most reaction effects are due to the same few triggers, its not worth implementing a trigger system that checks every effect and 
-- potentially arbitrarily `intercept`s the interpretation
-- That would support some arbitrary intercepting rules modifying gameplay, which is unnecessarily complex code permitted by the types.
-- Instead, we are going to hard code a mechanism for blocking attacks, and place several places where reactions will be checked for.
-- Possibilities:
-- 1. Listeners and event emitters
-- 2. Conditions and effects check for conditions/reactions via an (event/boolean check)
-- 3. ...adding checks in the state, I guess.
-- 4. Rule combinators/overriding/biased monoids
-- 5. Emit an event for an attack into a big events datatype


-- Mechanics:
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

wah :: Members '[BoardInit, PlayerIO, Stacks, Log] r => [Player] -> [CardFace] -> Sem r ()
wah pl cf =  evalState @GameState (initGS pl) .
             interpStateRead .
             interpCardEffects .
             interpStateWrite $ playGame pl cf

-- TODO: Log interception, reactions, add all cards, separate code better into files.

