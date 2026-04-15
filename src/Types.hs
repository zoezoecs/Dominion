{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO : Is this really the strat? c.f. design choice below
data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord, Show, Generic)
-- Design choice: all cards have ids and aren't just handled as cards.
data Card = MkCard Int CardFace deriving (Eq, Ord, Show, Generic)
newtype TempId = MkTempId Int deriving (Eq, Ord, Show, Generic)
newtype ObscuredCard = Obscured TempId deriving (Eq, Ord, Show, Generic)
type PotentiallyObscured = Either (Card, TempId) ObscuredCard

data CardTypes = CardAttack | CardReaction | CardAction | CardTreasure | CardVictory deriving (Eq, Ord, Show, Generic)
newtype Player = MkPlayer Int deriving (Ord, Eq, Show, Generic)

-- Obvious design choice: Representing errors and card positions as data
data InvalidMove = NoActions | CardPositionIncorrect deriving (Eq, Ord, Show, Generic)
data InvalidBuy = NoBuys | NoMoney | BadGain InvalidGain deriving (Eq, Ord, Show, Generic)
data InvalidGain = NotInKingdom | EmptySupply | GainError deriving (Eq, Ord, Show, Generic)
data TreasureError = NotATresure deriving (Eq, Ord, Show, Generic)
data InvalidReaction = NoCard | ConditionNotMet deriving (Eq, Ord, Show, Generic)

data PlayerPosition = PlayerDeck | PlayerDiscardPile | PlayerHand | PlayerInPlay | PlayerSetAside deriving (Eq, Ord, Show, Generic)



instance ToJSON CardFace where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CardFace
instance ToJSON Card where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Card
instance ToJSON Player where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Player
instance ToJSON InvalidBuy where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InvalidBuy
instance ToJSON InvalidGain where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InvalidGain
instance ToJSON TreasureError where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TreasureError
instance ToJSON InvalidReaction where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InvalidReaction
instance ToJSON PlayerPosition where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON PlayerPosition
instance ToJSON InvalidMove where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InvalidMove
instance ToJSON TempId where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TempId
instance ToJSON ObscuredCard where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ObscuredCard










-- data Kingdom = Kingdom
-- data Treasure = Treasure
-- data CurseSupply = CurseSupplye
-- data BasicSupply = TreasureSupply | VictorySupply | CurseSupply

-- Design choice: Maybe I just leave Kingdom/Treasure/Blah status to predicates?
-- If I break the card faces up into subsets its annoying to write "Gains a Copper"
-- But if I do this its a little annoying to say "Gain a Treasure"
-- c.f. Gain a treasure costing up to..
data Position = PlayerCard Player PlayerPosition | Supply CardFace | Trash deriving (Eq, Ord, Show, Generic)

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
