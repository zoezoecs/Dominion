{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Effects.Effects where

import Polysemy
import Polysemy.Input
import Data.ByteString.Lazy
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random.Stateful

import Base
import Types
import Effects.CardEffects
import Effects.PlayerIO
import Effects.Log



data Stacks m a where
  ActivePositions :: Stacks m [Position]
  GetStack :: Position -> Stacks m (Maybe [Card])
  ShuffleStack :: Position -> Stacks m ()
  StackOnto :: Position -> Position -> Stacks m ()
  DrawTo :: Position -> Position -> Stacks m (Maybe Card)
  CardToPos :: Card -> Position -> Stacks m ()
makeSem ''Stacks
deriving instance Show (Stacks m a)

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
deriving instance Show (BoardStateRead m a)

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
deriving instance Show (GameLoop m a)

data DoReaction m a where
  DoReaction :: Player -> Card -> ReactionEvent Card -> DoReaction m (Either InvalidReaction ())
makeSem ''DoReaction

data Reaction m a where
    BeforeReaction :: (forall m1 x. CardEffects (Sem m1) x -> Bool) -> m () -> Reaction m a
    AfterReaction :: (forall m1 x. CardEffects (Sem m1) x -> x -> Bool) -> m () -> Reaction m a
reactionMap :: (m1 () -> m2 ()) -> Reaction m1 a -> Reaction m2 a
reactionMap f (BeforeReaction cond m) = BeforeReaction cond (f m)
reactionMap f (AfterReaction cond m) = AfterReaction cond (f m)

data GameRules m a where
    CanBuy :: Player -> CardFace -> GameRules m (Either InvalidBuy ())
    CanAct :: Player -> Card -> GameRules m (Either InvalidMove ())
    CanTreasure :: Player -> Card -> GameRules m (Either TreasureError Int)
    CanReact :: Player -> Card -> ReactionEvent Card -> GameRules m (Either InvalidReaction HasReaction)
makeSem ''GameRules

-- Design choice: Messages to clients entirely through separate messages, and logs are reinterpreted from effects
-- Partial information managed by different messages with less information, no state tracking, no ability to refer to
-- previous messages for granular information (the card that was drawn 2 turns ago was a ...)
-- Clients don't reconstruct state, they just display the required information and collect the moves.
-- Clients don't see the card logic causality, they just see streams of events and must infer themselves.
-- How to make sure enough information gets through? We need a protocol.

data DataSerialised m a where
  DataIn :: DataSerialised m LazyByteString
  DataOut :: LazyByteString -> DataSerialised m ()
makeSem ''DataSerialised

data RandomShuffle m a where
    RandomShuffle :: [a] -> RandomShuffle m [a]
makeSem ''RandomShuffle
deriving instance Show a => Show (RandomShuffle m a)

data RandomUniqueId m a where
  RandomUniqueId :: RandomUniqueId m Int
makeSem ''RandomUniqueId
deriving instance Show (RandomUniqueId m a)

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


type CardSemantics' = forall r. Members [BoardStateRead, CardEffects, PlayerIO] r => Player -> Card -> Sem r ()
type CardReactionSemantics' = forall r. (Members '[CardEffects] r) => Player -> Card -> Reaction (Sem r) ()
newtype CardSemantics = CardSemantics {getSemantics :: CardSemantics'}
newtype CardReactionSemantics = CardReactionSemantics {getReactionSemantics :: CardReactionSemantics'}
