{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Effects where

import Polysemy
import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Base


-- Design choice: all cards have ids and aren't just handled as cards.
newtype Card = MkCard Int deriving Eq
data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord)
data CardTypes = CardAttack | CardReaction | CardAction | CardTreasure | CardVictory deriving Eq
newtype Player = MkPlayer Int deriving (Ord, Eq)

-- Obvious design choice: Representing errors and card positions as data
data InvalidMove = NoActions | CardPositionIncorrect
data InvalidBuy = NoMoney | BadGain InvalidGain
data InvalidGain = NotInKingdom | EmptySupply | GainError

data PlayerPosition = PlayerDeck | PlayerDiscardPile | PlayerHand | PlayerInPlay | PlayerSetAside
data Kingdom = Kingdom
data Treasure = Treasure
data CurseSupply = CurseSupplye
data BasicSupply = TreasureSupply | VictorySupply | CurseSupply
-- Design choice: Maybe I just leave Kingdom/Treasure/Blah status to predicates?
-- If I break the card faces up into subsets its annoying to write "Gains a Copper"
-- But if I do this its a little annoying to say "Gain a Treasure"
-- c.f. Gain a treasure costing up to..
data Position = PlayerCard Player PlayerPosition | Supply CardFace | Trash

allPositions :: [PlayerPosition]
allPositions = [PlayerDeck, PlayerDiscardPile, PlayerHand, PlayerInPlay, PlayerSetAside]

-- its not clear why we wouldn't just reinterpret straight into a state monad
data Stacks m a where
  ActiveKingdoms :: Stacks m [CardFace] -- TODO: abstraction barrier broken
  GetStack :: Position -> Stacks m [Card]
  ShuffleStack :: Position -> Stacks m ()
  StackOnto :: Position -> Position -> Stacks m ()
  DrawTo :: Position -> Position -> Stacks m (Maybe Card)
  CardToPos :: Card -> Position -> Stacks m ()
makeSem ''Stacks
-- Interface: you can initialise cards into various positions and then move them between positions. You can query for a cards location and query a position. 
-- You can draw a specific card out and place it back where it came from, or randomly in a deck.
-- How do we specify how to move them? How do we handle information?
-- Use random number generator, with drawing from top and bottom representing dirac generator?
-- Cards can be in ordered piles or unordered collections


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

data BoardStateEdit m a where
  StartingResources :: Player -> BoardStateEdit m ()
  BuyCard :: Player -> CardFace -> BoardStateEdit m (Either InvalidBuy Card)
  PlayFromHand :: Player -> Card -> BoardStateEdit m (Either InvalidMove ()) -- This is what should be used to check actions and membership in hand
  -- Design choice: Inline recovery function. c.f. Error, Either, Validation/token checking, state versioning, linearity, uuids
  DrawTurnStart :: Player -> Int -> BoardStateEdit m [Card] -- Draw from deck
  DiscardHandCleanup :: Player -> BoardStateEdit m ()
makeSem ''BoardStateEdit


data CardEffects m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects m Int
  ModifyBuys :: Int -> CardEffects m Int
  ModifyCurrency :: Int -> CardEffects m Int

  ActivateCard :: Player -> Card -> CardEffects m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects m (Maybe Card)
  BlockOne :: Player -> CardEffects m () -- Blocks the next attack? This could so lead to a bug lmao...
  Discard :: Player -> Card -> CardEffects m ()
  TrashCard :: Player -> Card -> CardEffects m ()
  Reveal :: Player -> Card -> CardEffects m ()
  TopDeck :: Player -> Card -> CardEffects m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects m (Either InvalidGain Card)
makeSem ''CardEffects

drawCard :: Member CardEffects r => Player -> Int -> Sem r [Card]
drawCard player n = fmap catMaybes $ replicateM n $ drawOnce player

gainCard :: Member CardEffects r => Player -> CardFace -> Sem r (Either InvalidGain Card)
gainCard pl cf = gainCardTo pl cf PlayerDiscardPile

applyTo :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
applyTo f xs = mapM f =<< xs

applyToOthers :: (Member CardEffects r, Member BoardStateRead r) => Player -> (Player -> Sem r a) -> Sem r (Map Player a)
applyToOthers player f = applyTo f (dupKey . Map.delete player <$> getPlayers)

data Log m a where
  LogPlayerRoundStart :: Player -> Log m ()
  LogBuy :: Player -> CardFace -> Log m Card
  LogAct :: Player -> Card -> Log m ()
  LogDraw :: Player -> Log m Card -- Remember some players wont get a log message with the card drawn.
  LogDiscard :: Player -> Log m Card
  LogReveal :: Player -> Card -> Log m Card
makeSem ''Log
-- Design choice: Messages to clients entirely through separate messages, and logs are reinterpreted from effects
-- Partial information managed by different messages with less information, no state tracking, no ability to refer to
-- previous messages for granular information (the card that was drawn 2 turns ago was a ...)
-- Clients don't reconstruct state, they just display the required information and collect the moves.
-- Clients don't see the card logic causality, they just see streams of events and must infer themselves.
-- How to make sure enough information gets through? We need a protocol.



data BoardInit m a where
  SetSupply :: Map CardFace Int -> BoardInit m ()
  SetHand :: Map CardFace Int -> BoardInit m () -- NOTE: DOES NOT INCLUDE COPPER? COPPER IS DRAWN FROM THE TOTAL, ESTATES ARENT.
makeSem ''BoardInit
-- This is a stupid effect

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetTrashAny :: Player -> [Card] -> PlayerIO m [Card]
  GetTrashExactlyN :: Player -> Int -> [Card] -> PlayerIO m [a]
  SendInfo :: Player -> PlayerIO m ()
makeSem ''PlayerIO

data Reaction m = Reaction (CardEffects m () -> Bool) (m ())
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
