{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Effects where

import Polysemy
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import Base


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

data Reaction m = Reaction (CardEffects m () -> Bool) (m ())

data Log m a where
  LogPlayerRoundStart :: Player -> Log m ()
  LogBuy :: Player -> CardFace -> Log m Card
  LogAct :: Player -> Card -> Log m ()
  LogDraw :: Player -> Log m Card -- Remember some players wont get a log message with the card drawn.
  LogDiscard :: Player -> Log m Card
  LogReveal :: Player -> Card -> Log m Card
makeSem ''Log

data BoardInit m a where
  SetSupply :: Map CardFace Int -> BoardInit m ()
  SetHand :: Map CardFace Int -> BoardInit m () -- NOTE: DOES NOT INCLUDE COPPER? COPPER IS DRAWN FROM THE TOTAL, ESTATES ARENT.
makeSem ''BoardInit

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetTrashAny :: Player -> [Card] -> PlayerIO m [Card]
  GetTrashExactlyN :: Player -> Int -> [Card] -> PlayerIO m [a]
  SendInfo :: Player -> PlayerIO m ()
makeSem ''PlayerIO

type CardSemantics = forall r. Members [BoardStateRead, CardEffects, PlayerIO] r => Player -> Card -> Sem r ()

-- Obvious design choice: state is a big datatype
data GameState = MkGameState {
  players :: [Player],
  blocks :: Map Player Bool,
  current_player :: Player,
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