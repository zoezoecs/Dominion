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
import Internal.TH


-- TODO : Is this really the strat? c.f. design choice below
data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord, Show)
-- Design choice: all cards have ids and aren't just handled as cards.
data Card = MkCard Int CardFace deriving (Eq, Ord, Show)
newtype TempId = MkTempId Int deriving (Eq, Ord, Show)
data ObscuredCard = Either (Card, Maybe TempId) (CardFace, TempId) deriving (Eq, Ord, Show)
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

-- Obvious design choice: state is a big datatype
data GameState = MkGameState {
  all_players :: [Player],
  blocks :: Map Player Bool,
  current_actions :: Int,
  current_buys :: Int,
  current_currency :: Int
  -- reactions :: [Reaction m]
}
modActions n gs = gs{current_actions=n+current_actions gs}
modBuys n gs = gs{current_buys=n+current_buys gs}
modCurrency n gs = gs{current_currency=n+current_currency gs}

setBlocks :: Player -> Bool -> GameState -> GameState
setBlocks pl b gs = gs{blocks=Map.insert pl b (blocks gs)}

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
  DrawOnce :: Player -> CardEffects' card m (Maybe card)  -- Note Maybe signals no cards in draw OR discard
  BlockOne :: Player -> card -> CardEffects' card m () -- Blocks the next attack? This could so lead to a bug lmao...
  Discard :: Player -> card -> CardEffects' card m () -- NOTE: None of these are "discard FROM HAND" or anything
  TrashCard :: Player -> card -> CardEffects' card m ()
  Reveal :: Player -> card -> CardEffects' card m ()
  TopDeck :: Player -> card -> CardEffects' card m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects' card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffects'
deriving instance (Show a, Show card) => Show (CardEffects' card m a)
type CardEffects = CardEffects' Card

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


-- POLYSEMY ANNOYING:
-- We have to monomorphise here for a = CardEffects m a to avoid Polysemy thinking Loggable (CardEffects m a) is higher order.
-- We also need this thing to carry around the proof that the output of CardEffects m a is always showable, because we know the constructors.
-- This allows us to have a LogEffect constructor in Log.
data Loggable card a where
  LogEvent :: (Show a, Show card) => CardEffects' card m a -> Loggable card a
deriving instance Show (Loggable card a)

-- This is inspecting each constructor to see that there must implicitly be a Show a for each a
-- It looks like its doing nothing, but its actually implicitly packing a Show instance dict
showLoggable :: CardEffects r a -> Loggable Card a
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

data Log card m a where
  LogPlayerRoundStart :: Player -> Log card m ()
  LogBuy :: Player -> CardFace -> Log card m ()
  LogAct :: Show card => Player -> card -> Log card m ()
  LogTreasure :: Show card => Player -> card -> Log card m ()
  LogEffect :: Loggable card a -> a -> Log card m a
makeSemMonomorphised ''Card ''Log

data LogToPlayer m a where
  LogToPlayer :: Log card m a -> Player -> LogToPlayer m a
makeSem ''LogToPlayer

deriving instance (Show a, Show card) => Show (Log card m a)

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
