{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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



import Types

import Data.Aeson
import Data.Aeson.GADT.TH

import Data.Constraint.Extras

import Data.Type.Equality
import Data.GADT.Compare
import Data.Some.Newtype

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

data CardEffects' card m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects' card m Int
  ModifyBuys :: Int -> CardEffects' card m Int
  ModifyCurrency :: Int -> CardEffects' card m Int

  ActivateCard :: Player -> card -> CardEffects' card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects' card m (Maybe card)  -- Note Maybe signals no cards in both draw AND discard
  BlockOne :: Player -> card -> CardEffects' card m () -- Blocks the next attack
  Discard :: Player -> card -> CardEffects' card m () -- NOTE: None of these are "discard FROM HAND" or anything
  TrashCard :: Player -> card -> CardEffects' card m ()
  Reveal :: Player -> card -> CardEffects' card m ()
  TopDeck :: Player -> card -> CardEffects' card m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects' card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffects'
deriving instance (Show a, Show card) => Show (CardEffects' card m a)
deriving instance (Eq card) => Eq (CardEffects' card m a)
type CardEffects = CardEffects' Card

cardEffectrMap :: CardEffects' card r1 a -> CardEffects' card r2 a
cardEffectrMap (ModifyActions n) = ModifyActions n
cardEffectrMap (ModifyBuys n) = ModifyBuys n
cardEffectrMap (ModifyCurrency n) = ModifyCurrency n

cardEffectrMap (ActivateCard pl c) = ActivateCard pl c
cardEffectrMap (DrawOnce pl) = DrawOnce pl
cardEffectrMap (BlockOne pl c) = BlockOne pl c
cardEffectrMap (Discard pl c) = Discard pl c
cardEffectrMap (TrashCard pl c) = TrashCard pl c
cardEffectrMap (Reveal pl c) = Reveal pl c
cardEffectrMap (TopDeck pl c) = TopDeck pl c
cardEffectrMap (GainCardTo pl cf pp) = GainCardTo pl cf pp

deriveJSONGADT ''CardEffects'
instance (c Int, c (), c (Maybe card), c (Either InvalidGain card)) 
    => Has c (CardEffects' card m) where
  has eff k = case eff of
    ModifyActions{}  -> k
    ModifyBuys{}     -> k
    ModifyCurrency{} -> k
    DrawOnce{}       -> k
    GainCardTo{}     -> k
    ActivateCard{}   -> k
    BlockOne{}       -> k
    Discard{}        -> k
    TrashCard{}      -> k
    Reveal{}         -> k
    TopDeck{}        -> k

instance Eq card => GEq (CardEffects' card m) where
  geq (ModifyActions n1) (ModifyActions n2) = if n1 == n2 then Just Refl else Nothing
  geq (ModifyBuys n1) (ModifyBuys n2) = if n1 == n2 then Just Refl else Nothing
  geq (ModifyCurrency n1) (ModifyCurrency n2) = if n1 == n2 then Just Refl else Nothing
  geq (ActivateCard p1 c1) (ActivateCard p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (DrawOnce p1) (DrawOnce p2) = if p1 == p2 then Just Refl else Nothing
  geq (BlockOne p1 c1) (BlockOne p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (Discard p1 c1) (Discard p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (TrashCard p1 c1) (TrashCard p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (Reveal p1 c1) (Reveal p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (TopDeck p1 c1) (TopDeck p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (GainCardTo p1 f1 pos1) (GainCardTo p2 f2 pos2) = if p1 == p2 && f1 == f2 && pos1 == pos2 then Just Refl else Nothing
  geq _ _ = Nothing


data LoggedEvent card = forall m a. LogEvent (CardEffects' card m a) a

instance Functor LoggedEvent where
  fmap _ (LogEvent (ModifyActions n) x) = LogEvent (ModifyActions n) x
  fmap _ (LogEvent (ModifyBuys n) x) = LogEvent (ModifyBuys n) x
  fmap _ (LogEvent (ModifyCurrency n) x) = LogEvent (ModifyCurrency n) x
  fmap f (LogEvent (ActivateCard pl c) x) = LogEvent (ActivateCard pl (f c)) x
  fmap f (LogEvent (DrawOnce pl) x) = LogEvent (DrawOnce pl) (fmap f x)
  fmap f (LogEvent (BlockOne pl c) x) = LogEvent (BlockOne pl (f c)) x
  fmap f (LogEvent (Discard pl c) x) = LogEvent (Discard pl (f c)) x
  fmap f (LogEvent (TrashCard pl c) x) = LogEvent (TrashCard pl (f c)) x
  fmap f (LogEvent (Reveal pl c) x) = LogEvent (Reveal pl (f c)) x
  fmap f (LogEvent (TopDeck pl c) x) = LogEvent (TopDeck pl (f c)) x
  fmap f (LogEvent (GainCardTo pl cf pp) x) = LogEvent (GainCardTo pl cf pp) (fmap f x)

instance ToJSON card => ToJSON (LoggedEvent card) where
  toJSON (LogEvent eff result) =
    has @ToJSON eff $ object
      [ "effect" .= toJSON eff
      , "result" .= toJSON result
      ]

instance FromJSON card => FromJSON (LoggedEvent card) where
  parseJSON = withObject "LoggedEvent" $ \o -> do
    Some eff <- o .: "effect"
    has @FromJSON eff $ do
      result <- parseJSON =<< o .: "result"
      pure (LogEvent eff result)

instance Eq card => Eq (LoggedEvent card) where
  LogEvent eff1 result1 == LogEvent eff2 result2 =
    case geq eff1 (cardEffectrMap eff2) of
      Nothing   -> False
      Just Refl -> has @Eq eff1 $ result1 == result2

instance Show card => Show (LoggedEvent card) where
  show (LogEvent eff result) =
    has @Show eff $ "LogEvent (" <> show eff <> ") (" <> show result <> ")"



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


data Log card m a where
  LogPlayerRoundStart :: Player -> Log card m ()
  LogBuy :: Player -> CardFace -> Log card m ()
  LogAct :: Player -> card -> Log card m ()
  LogTreasure :: Player -> card -> Log card m ()
  LogEffect :: LoggedEvent card -> Log card m ()
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
deriveJSONGADT ''Log

logCardMap :: (c1 -> c2) -> Log c1 m a -> Log c2 m a
logCardMap f (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap f (LogBuy pl cf) = LogBuy pl cf
logCardMap f (LogAct pl c) = LogAct pl (f c)
logCardMap f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap f (LogEffect eff) = LogEffect (fmap f eff)

data LogToPlayer card m a where
  LogToPlayer :: Log card m () -> Player -> LogToPlayer card m ()
makeSem ''LogToPlayer

-- deriving instance (Show a, Show card) => Show (Log card m a)

data Obscure m a where
  GetTempId :: Card -> Obscure m TempId
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

data Reaction m a where
    BeforeReaction :: (forall m1 x. CardEffects (Sem m1) x -> Bool) -> m () -> Reaction m a
    AfterReaction :: (forall m1 x. CardEffects (Sem m1) x -> x -> Bool) -> m () -> Reaction m a

getReactionMonad :: Reaction m a -> m ()
getReactionMonad (BeforeReaction cond m) = m
getReactionMonad (AfterReaction cond m) = m

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
