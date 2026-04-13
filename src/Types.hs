module Types where

import Data.Map (Map)
import qualified Data.Map as Map

-- TODO : Is this really the strat? c.f. design choice below
data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord, Show)
-- Design choice: all cards have ids and aren't just handled as cards.
data Card = MkCard Int CardFace deriving (Eq, Ord, Show)
newtype TempId = MkTempId Int deriving (Eq, Ord, Show)
newtype ObscuredCard = Obscured TempId deriving (Eq, Ord, Show)
type PotentiallyObscured = Either (Card, TempId) ObscuredCard

data CardTypes = CardAttack | CardReaction | CardAction | CardTreasure | CardVictory deriving (Eq, Ord)
newtype Player = MkPlayer Int deriving (Ord, Eq, Show)

-- Obvious design choice: Representing errors and card positions as data
data InvalidMove = NoActions | CardPositionIncorrect deriving Show
data InvalidBuy = NoBuys | NoMoney | BadGain InvalidGain deriving Show
data InvalidGain = NotInKingdom | EmptySupply | GainError deriving Show
data TreasureError = NotATresure deriving Show
data InvalidReaction = NoCard | ConditionNotMet

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
