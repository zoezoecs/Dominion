{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, StandaloneDeriving, DeriveFunctor #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Effects where

import Polysemy
import Polysemy.Input
import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random.Stateful

import Base
import Internal.TH
import Types

data Stacks m a where
  ActivePositions :: Stacks m [Position] -- TODO: abstraction barrier broken
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
  -- IsValidCardPlay :: Player -> Card -> BoardStateRead m (Either InvalidMove ())
  -- GetReactions :: Player -> BoardStateRead m [Reaction m]
makeSem ''BoardStateRead

data GameLoop m a where
  StartingResources :: Player -> GameLoop m ()
  BuyCard :: Player -> CardFace -> GameLoop m (Either InvalidBuy Card)
  PlayFromHand :: Player -> Card -> GameLoop m (Either InvalidMove ()) -- This is what should be used to check actions and membership in hand
  PlayTreasure :: Player -> Card -> GameLoop m (Either TreasureError Int)
  -- Design choice: Inline recovery function. c.f. Error, Either, Validation/token checking, state versioning, linearity, uuids
  DrawTurnStart :: Player -> Int -> GameLoop m [Card] -- Draw from deck
  DiscardHandCleanup :: Player -> GameLoop m ()
makeSem ''GameLoop

data GameRules m a where
    CanBuy :: Player -> CardFace -> GameRules m (Either InvalidBuy ())
    CanAct :: Player -> Card -> GameRules m (Either InvalidMove ())
makeSem ''GameRules

-- POLYSEMY ANNOYING: I really want to make Card polymorphic, but this raises so many type inference issues.
-- I can get makeSem to work if I add polysemy-plugin, but the smart constructors are still complaining about
-- inability to infer the card membership. 
-- Most wrapper ideas make pattern matching and interpretation kind of annoying, and sometimes change smart constructor names.
data CardEffects' card m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects' card m Int
  ModifyBuys :: Int -> CardEffects' card m Int
  ModifyCurrency :: Int -> CardEffects' card m Int

  ActivateCard :: Player -> card -> CardEffects' card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects' card m (Maybe card)  -- Note Maybe signals no cards in both draw AND discard
  BlockOne :: Player -> card -> CardEffects' card m () -- Blocks the next attack? This could so lead to a bug lmao...
  Discard :: Player -> card -> CardEffects' card m () -- NOTE: None of these are "discard FROM HAND" or anything
  TrashCard :: Player -> card -> CardEffects' card m ()
  Reveal :: Player -> card -> CardEffects' card m ()
  TopDeck :: Player -> card -> CardEffects' card m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects' card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffects'
deriving instance (Show a, Show card) => Show (CardEffects' card m a)
type CardEffects = CardEffects' Card

newtype Answer a = Ans {getAns :: a} deriving (Eq, Ord, Show, Functor)
data CardEffects'' card where
  -- Modify game resources
  XModifyActions ::  Int  -> Answer Int -> CardEffects'' card
  XModifyBuys ::  Int  -> Answer Int -> CardEffects'' card
  XModifyCurrency ::  Int  -> Answer Int -> CardEffects'' card

  XActivateCard ::  Player -> card  -> Answer () -> CardEffects'' card
  XDrawOnce ::  Player  -> Answer (Maybe card) -> CardEffects'' card
  XBlockOne ::  Player -> card  -> Answer () -> CardEffects'' card 
  XDiscard ::  Player -> card  -> Answer () -> CardEffects'' card
  XTrashCard ::  Player -> card  -> Answer () -> CardEffects'' card
  XReveal ::  Player -> card  -> Answer () -> CardEffects'' card
  XTopDeck ::  Player -> card  -> Answer () -> CardEffects'' card
  XGainCardTo ::  Player -> CardFace -> PlayerPosition  -> Answer (Either InvalidGain card) -> CardEffects'' card
deriving instance Show card => Show (CardEffects'' card)
deriving instance Functor CardEffects''

removeAParameter :: CardEffects' card m a -> a -> CardEffects'' card
removeAParameter ((ModifyActions n)) x = XModifyActions n (Ans x)
removeAParameter ((ModifyBuys n)) x = XModifyBuys n (Ans x)
removeAParameter ((ModifyCurrency n)) x = XModifyCurrency n (Ans x)
removeAParameter ((ActivateCard pl c)) x = XActivateCard pl c (Ans x)
removeAParameter ((DrawOnce pl)) x = XDrawOnce pl (Ans x)
removeAParameter ((BlockOne pl c)) x = XBlockOne pl c (Ans x)
removeAParameter ((Discard pl c)) x = XDiscard pl c (Ans x)
removeAParameter ((TrashCard pl c)) x = XTrashCard pl c (Ans x)
removeAParameter ((Reveal pl c)) x = XReveal pl c (Ans x)
removeAParameter ((TopDeck pl c)) x = XTopDeck pl c (Ans x)
removeAParameter ((GainCardTo pl cf pos)) x = XGainCardTo pl cf pos (Ans x)

drawCard :: Member CardEffects r => Player -> Int -> Sem r [Card]
drawCard player n = fmap catMaybes $ replicateM n $ drawOnce player

gainCard :: Member CardEffects r => Player -> CardFace -> Sem r (Either InvalidGain Card)
gainCard pl cf = gainCardTo pl cf PlayerDiscardPile

applyTo :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
applyTo f xs = mapM f =<< xs

applyToOthers :: (Member BoardStateRead r) => Player -> (Player -> Sem r a) -> Sem r (Map Player a)
applyToOthers player f = applyTo f (dupKey . Map.delete player <$> getPlayers)

applyToAll :: (Member BoardStateRead r) => (Player -> Sem r a) -> Sem r (Map Player a)
applyToAll f = applyTo f (dupKey <$> getPlayers)


data Log card m a where
  LogPlayerRoundStart :: Player -> Log card m ()
  LogBuy :: Player -> CardFace -> Log card m ()
  LogAct :: Show card => Player -> card -> Log card m ()
  LogTreasure :: Show card => Player -> card -> Log card m ()
  LogEffect :: (Show card) => CardEffects'' card -> Log card m ()
  -- POLYSEMY ANNOYING: This constructor runs in to a _lot_ of issues with unifying the `a` type variable when we try to map the card variable
  -- to inject into an Either type. We can give up the type level guarantee that the result of an effect matches the actual effect, since we
  -- have to do lots of boilerplate enumeration anyways for everything. If we could make the interpreters have boilerplate free TH free code,
  -- we might be able to come back here and fix this.
  -- LogModifyActions :: Answer Int -> Int -> Log card m ()
  -- LogModifyBuys :: Answer Int -> Int -> Log card m ()
  -- LogModifyCurrency :: Answer Int -> Int -> Log card m ()
  -- LogActivateCard :: Player -> card -> Log card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- LogDrawOnce :: Answer (Maybe card) -> Player -> Log card m ()  -- Note Maybe signals no cards in both draw AND discard
  -- LogBlockOne :: Player -> card -> Log card m () -- Blocks the next attack? This could so lead to a bug lmao...
  -- LogDiscard :: Player -> card -> Log card m () -- NOTE: None of these are "discard FROM HAND" or anything
  -- LogTrashCard :: Player -> card -> Log card m ()
  -- LogReveal :: Player -> card -> Log card m ()
  -- LogTopDeck :: Player -> card -> Log card m ()
  -- LogGainCardTo :: Answer (Either InvalidGain card) -> Player -> CardFace -> PlayerPosition -> Log card m ()
makeSemMonomorphised ''Card ''Log

logCardMap :: (Show c2) => (c1 -> c2) -> Log c1 m a -> Log c2 m a
logCardMap f (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap f (LogBuy pl cf) = LogBuy pl cf
logCardMap f (LogAct pl c) = LogAct pl (f c)
logCardMap f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap f (LogEffect eff) = LogEffect (fmap f eff)

data LogToPlayer card m a where
  LogToPlayer :: Log card m a -> Player -> LogToPlayer card m a
makeSem ''LogToPlayer

deriving instance (Show a, Show card) => Show (Log card m a)

newtype TempIdMap = TempIdMap (Map Card Int)

data Obscure m a where
  Obscure :: Card -> Obscure m ObscuredCard
makeSem ''Obscure

data Correlation m a where
  MkCorrelation :: m a -> Correlation m a
makeSem ''Correlation



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
makeSem ''PlayerIO

data Reaction m a where
    Reaction :: (CardEffects m () -> Bool) -> m () -> Reaction m a
makeSem ''Reaction

data RandomShuffle m a where
    RandomShuffle :: [a] -> RandomShuffle m [a]
makeSem ''RandomShuffle

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
