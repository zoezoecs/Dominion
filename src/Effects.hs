{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, StandaloneDeriving #-}
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


-- TODO : Is this really the strat? c.f. design choice below
data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord, Show)
-- Design choice: all cards have ids and aren't just handled as cards.
data Card = MkCard Int CardFace deriving (Eq, Ord, Show)
newtype TempId = MkTempId Int
data ObscuredCard = Card | TempId
-- Design choice: We have an obscuredcard type to avoid polymorphism issues with generalising cards
data CardTypes = CardAttack | CardReaction | CardAction | CardTreasure | CardVictory deriving (Eq, Ord)
newtype Player = MkPlayer Int deriving (Ord, Eq, Show)

-- Obvious design choice: Representing errors and card positions as data
data InvalidMove = NoActions | CardPositionIncorrect deriving Show
data InvalidBuy = NoBuys | NoMoney | BadGain InvalidGain deriving Show
data InvalidGain = NotInKingdom | EmptySupply | GainError deriving Show
data TreasureError = NotATresure deriving Show

data PlayerPosition = PlayerDeck | PlayerDiscardPile | PlayerHand | PlayerInPlay | PlayerSetAside deriving (Eq, Ord, Show)
-- data Kingdom = Kingdom
-- data Treasure = Treasure
-- data CurseSupply = CurseSupplye
-- data BasicSupply = TreasureSupply | VictorySupply | CurseSupply

-- Design choice: Maybe I just leave Kingdom/Treasure/Blah status to predicates?
-- If I break the card faces up into subsets its annoying to write "Gains a Copper"
-- But if I do this its a little annoying to say "Gain a Treasure"
-- c.f. Gain a treasure costing up to..
data Position = PlayerCard Player PlayerPosition | Supply CardFace | Trash deriving (Eq, Ord, Show)

allPositions :: [PlayerPosition]
allPositions = [PlayerDeck, PlayerDiscardPile, PlayerHand, PlayerInPlay, PlayerSetAside]

-- its not clear why we wouldn't just reinterpret straight into a state monad
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

isSupply (Supply c) = Just c
isSupply _ = Nothing

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
-- 
data CardEffectss card m a where
  -- Modify game resources
  WahModifyActions :: Int -> CardEffectss card m Int
  WahModifyBuys :: Int -> CardEffectss card m Int
  WahModifyCurrency :: Int -> CardEffectss card m Int

  WahActivateCard :: Player -> card -> CardEffectss card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  WahDrawOnce :: Player -> CardEffectss card m (Maybe card)  -- Note Maybe signals no cards in draw OR discard
  WahBlockOne :: Player -> card -> CardEffectss card m () -- Blocks the next attack? This could so lead to a bug lmao...
  WahDiscard :: Player -> card -> CardEffectss card m () -- NOTE: None of these are "discard FROM HAND" or anything
  WahTrashCard :: Player -> card -> CardEffectss card m ()
  WahReveal :: Player -> card -> CardEffectss card m ()
  WahTopDeck :: Player -> card -> CardEffectss card m ()
  WahGainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffectss card m (Either InvalidGain card)
makeSem ''CardEffectss
type CardEffecte = CardEffectss Card
modifyActions' :: Member CardEffecte r => Int -> Sem r Int
modifyActions' = wahModifyActions @Card

-- POLYSEMY ANNOYING: I really want to make Card polymorphic, but this raises so many type inference issues.
-- I can get makeSem to work if I add polysemy-plugin, but the smart constructors are still complaining about
-- inability to infer the card membership. 
-- Most wrapper ideas make pattern matching and interpretation kind of annoying, and sometimes change smart constructor names.
data CardEffects m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects m Int
  ModifyBuys :: Int -> CardEffects m Int
  ModifyCurrency :: Int -> CardEffects m Int

  ActivateCard :: Player -> Card -> CardEffects m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects m (Maybe Card)  -- Note Maybe signals no cards in draw OR discard
  BlockOne :: Player -> Card -> CardEffects m () -- Blocks the next attack? This could so lead to a bug lmao...
  Discard :: Player -> Card -> CardEffects m () -- NOTE: None of these are "discard FROM HAND" or anything
  TrashCard :: Player -> Card -> CardEffects m ()
  Reveal :: Player -> Card -> CardEffects m ()
  TopDeck :: Player -> Card -> CardEffects m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects m (Either InvalidGain Card)
makeSem ''CardEffects

deriving instance Show a => Show (CardEffects m a)

drawCard :: Member CardEffects r => Player -> Int -> Sem r [Card]
drawCard player n = fmap catMaybes $ replicateM n $ drawOnce player

gainCard :: Member CardEffects r => Player -> CardFace -> Sem r (Either InvalidGain Card)
gainCard pl cf = gainCardTo pl cf PlayerDiscardPile

applyTo :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
applyTo f xs = mapM f =<< xs

applyToOthers :: (Member CardEffects r, Member BoardStateRead r) => Player -> (Player -> Sem r a) -> Sem r (Map Player a)
applyToOthers player f = applyTo f (dupKey . Map.delete player <$> getPlayers)

-- POLYSEMY ANNOYING:
-- We have to monomorphise here for a = CardEffects m a to avoid Polysemy thinking Loggable (CardEffects m a) is higher order.
-- We also need this thing to carry around the proof that the output of CardEffects m a is always showable, because we know the constructors.
-- This allows us to have a LogEffect constructor in Log.
data Loggable a where
  LogEvent :: Show a => CardEffects m a -> Loggable a
deriving instance Show (Loggable a)

-- This is inspecting each constructor to see that there must implicitly be a Show a for each a
-- It looks like its doing nothing, but its actually implicitly packing a Show instance dict
showLoggable :: CardEffects r a -> Loggable a
showLoggable (ModifyActions n) = LogEvent (ModifyActions n)
showLoggable (ModifyBuys n) = LogEvent (ModifyBuys n)
showLoggable (ModifyCurrency n) = LogEvent (ModifyCurrency n)
showLoggable (ActivateCard pl c) = LogEvent (ActivateCard pl c)
showLoggable (DrawOnce pl) = LogEvent (DrawOnce pl)
showLoggable (BlockOne pl c) = LogEvent (BlockOne pl c)
showLoggable (Discard pl c) = LogEvent (Discard pl c)
showLoggable (TrashCard pl c) = LogEvent (TrashCard pl c)
showLoggable (Reveal pl c) = LogEvent (Reveal pl c)
showLoggable (TopDeck pl c) = LogEvent (TopDeck pl c)
showLoggable (GainCardTo pl c pos) = LogEvent (GainCardTo pl c pos)

data Log m a where
  LogPlayerRoundStart :: Player -> Log m ()
  LogBuy :: Player -> CardFace -> Log m ()
  LogAct :: Player -> Card -> Log m ()
  LogTreasure :: Player -> Card -> Log m ()
  LogEffect :: Loggable a -> a -> Log m a
makeSem ''Log

deriving instance Show a => Show (Log m a)

newtype TempIdMap = TempIdMap (Map Card Int)
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
