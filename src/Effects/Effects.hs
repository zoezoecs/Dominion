{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Effects.Effects where

import Polysemy
import Polysemy.Input
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random.Stateful

import Base
import Types
import Effects.CardEffects



data Stacks m a where
  ActivePositions :: Stacks m [Position]
  GetStack :: Position -> Stacks m (Maybe [Card])
  ShuffleStack :: Position -> Stacks m ()
  StackOnto :: Position -> Position -> Stacks m ()
  DrawTo :: Position -> Position -> Stacks m (Maybe Card)
  CardToPos :: Card -> Position -> Stacks m ()
makeSem ''Stacks

data PileConfig t = PileConfig
  { refillFrom  :: Map Position Position,   -- when empty, refill from here
    shuffleOnRefill :: t Position}          -- shuffle after refilling

isSupply :: Position -> Maybe CardFace
isSupply (Supply c) = Just c
isSupply _ = Nothing

getSupplies :: [Position] -> [CardFace]
getSupplies = mapMaybe isSupply

activeKingdoms :: Member Stacks r => Sem r [CardFace]
activeKingdoms = getSupplies <$> activePositions

data BoardStateRead m a where
  GetPlayers :: BoardStateRead m (Map Player ())
  GetVP :: Player -> BoardStateRead m Int
  GetHand :: Player -> BoardStateRead m [Card]
  GetDeck :: Player -> BoardStateRead m [Card]
  GetTopCard :: Player -> BoardStateRead m (Maybe Card)
  GetTopNCard :: Player -> Int -> BoardStateRead m (Maybe Card)
  GetDiscardPile :: Player -> BoardStateRead m [Card]
  IsGameOver :: BoardStateRead m Bool
makeSem ''BoardStateRead

applyTo :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
applyTo f xs = mapM f =<< xs

applyToOthers :: (Member BoardStateRead r) => Player -> (Player -> Sem r a) -> Sem r (Map Player a)
applyToOthers player f = applyTo f (dupKey . Map.delete player <$> getPlayers)

applyToAll :: (Member BoardStateRead r) => (Player -> Sem r a) -> Sem r (Map Player a)
applyToAll f = applyTo f (dupKey <$> getPlayers)


data GameLoop m a where
  StartingResources :: Player -> GameLoop m ()
  BuyCard :: Player -> CardFace -> GameLoop m (Either InvalidBuy Card)
  PlayFromHand :: Player -> Card -> GameLoop m (Either InvalidMove ()) -- This is what should be used to check actions and membership in hand
  PlayTreasure :: Player -> Card -> GameLoop m (Either TreasureError Int)
  DrawTurnStart :: Player -> Int -> GameLoop m [Card] -- Draw from deck
  DiscardHandCleanup :: Player -> GameLoop m ()
makeSem ''GameLoop

data DoReaction m a where
  DoReaction :: Player -> Card -> (forall r. CardEffects r a) -> Maybe a -> DoReaction m (Either InvalidReaction ())
makeSem ''DoReaction

data GameRules m a where
    CanBuy :: Player -> CardFace -> GameRules m (Either InvalidBuy ())
    CanAct :: Player -> Card -> GameRules m (Either InvalidMove ())
    CanReact :: Player -> Card -> (forall r. CardEffects r a) -> Maybe a -> GameRules m (Either InvalidReaction ())
makeSem ''GameRules

data Reaction m a where
    BeforeReaction :: (forall m1 x. CardEffects (Sem m1) x -> Bool) -> m () -> Reaction m a
    AfterReaction :: (forall m1 x. CardEffects (Sem m1) x -> x -> Bool) -> m () -> Reaction m a

getReactionMonad :: Reaction m a -> m ()
getReactionMonad (BeforeReaction cond m) = m
getReactionMonad (AfterReaction cond m) = m

-- Design choice: Messages to clients entirely through separate messages, and logs are reinterpreted from effects
-- Partial information managed by different messages with less information, no state tracking, no ability to refer to
-- previous messages for granular information (the card that was drawn 2 turns ago was a ...)
-- Clients don't reconstruct state, they just display the required information and collect the moves.
-- Clients don't see the card logic causality, they just see streams of events and must infer themselves.
-- How to make sure enough information gets through? We need a protocol.

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetPlayTreasure :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetTrashAny :: Player -> [Card] -> PlayerIO m [Card]
  GetTrashExactlyN :: Player -> Int -> [Card] -> PlayerIO m [a]
  SendInfo :: Player -> PlayerIO m ()
  GetPlayerReaction :: Player -> (forall r. CardEffects r a) -> Maybe a -> PlayerIO m (Maybe Card)
makeSem ''PlayerIO

data RandomShuffle m a where
    RandomShuffle :: [a] -> RandomShuffle m [a]
makeSem ''RandomShuffle

data RandomUniqueId m a where
  RandomUniqueId :: RandomUniqueId m Int
makeSem ''RandomUniqueId

-- POLYSEMY ANNOYING:
-- More packing so the constraint goes through...
data HoldRandom m where
    HoldRandom :: StatefulGen gen m => gen -> HoldRandom m

type RandomGenEff m = Input (HoldRandom m)
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


type CardSemantics = forall r. Members [BoardStateRead, CardEffects, PlayerIO] r => Player -> Card -> Sem r ()
