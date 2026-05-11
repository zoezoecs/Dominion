{-# LANGUAGE DeriveGeneric, DerivingVia #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map

data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord, Show, Generic)
-- Design choice: all cards have ids and aren't just handled as cards.
data Card = MkCard Int CardFace deriving (Eq, Ord, Show, Generic)
newtype TempId = MkTempId Int deriving (Eq, Ord, Show, Generic)
newtype ObscuredCard = Obscured TempId deriving (Eq, Ord, Show, Generic)
newtype PotentiallyObscured = PObscured (Either (Card, TempId) ObscuredCard) deriving (Eq, Ord, Show, Generic)

data CardTypes = CardAttack | CardReaction | CardAction | CardTreasure | CardVictory | CardCurse deriving (Eq, Ord, Show, Generic)
newtype Player = MkPlayer Int deriving (Ord, Eq, Show, Generic)

-- Obvious design choice: Representing errors and card positions as data
data InvalidMove = NoActions | CardPositionIncorrect | NotAnAction deriving (Eq, Ord, Show, Generic)
data InvalidBuy = NoBuys | NoMoney | BadGain InvalidGain deriving (Eq, Ord, Show, Generic)
data InvalidGain = NotInKingdom | EmptySupply | GainError deriving (Eq, Ord, Show, Generic)
data TreasureError = TreasurePositionIncorrect | NotATresure deriving (Eq, Ord, Show, Generic)
data InvalidReaction = NoCard | ConditionNotMet | NoReaction deriving (Eq, Ord, Show, Generic)

data PlayerPosition = PlayerDeck | PlayerDiscardPile | PlayerHand | PlayerInPlay | PlayerSetAside deriving (Eq, Ord, Show, Generic)

-- This wouldn't actually implement merchant semantics
data CurrencyPoints = CurrencyPoints {getPlainCurrency :: Int, getMerchantCurrency :: Int} deriving (Eq, Ord, Show, Generic)
instance Semigroup CurrencyPoints where
    CurrencyPoints n1 m1 <> CurrencyPoints n2 m2 = CurrencyPoints (n1 + n2) (m1 + m2)
instance Monoid CurrencyPoints where
    mempty = CurrencyPoints 0 0

plainCurrency :: Int -> CurrencyPoints
plainCurrency n = CurrencyPoints n 0

data VictoryPoints = VictoryPoints {getPlainVP :: Int, getGardensVP :: Int} deriving (Eq, Ord, Show, Generic)
instance Semigroup VictoryPoints where
    VictoryPoints n1 m1 <> VictoryPoints n2 m2 = VictoryPoints (n1 + n2) (m1 + m2)
instance Monoid VictoryPoints where
    mempty = VictoryPoints 0 0
-- data VictoryPointsType = PlainVP | GardensVP deriving (Eq, Ord, Show, Generic)
-- newtype VictoryPoints = VPS [VictoryPointsType] deriving (Eq, Ord, Show, Generic)
-- deriving via [VictoryPointsType] instance Semigroup VictoryPoints
-- deriving via [VictoryPointsType] instance Monoid VictoryPoints

plainVP :: Int -> VictoryPoints
plainVP n = VictoryPoints n 0

gardensVP :: Int -> VictoryPoints
gardensVP = VictoryPoints 0

-- Note: Kingdom vs non-kingdom cards aren't separated structurally.
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
} deriving (Eq, Ord, Show)

modActions :: Int -> GameState -> GameState
modActions n gs = gs{current_actions=n+current_actions gs}
modBuys :: Int -> GameState -> GameState
modBuys n gs = gs{current_buys=n+current_buys gs}
modCurrency :: Int -> GameState -> GameState
modCurrency n gs = gs{current_currency=n+current_currency gs}

setBlocks :: Player -> Bool -> GameState -> GameState
setBlocks pl b gs = gs{blocks=Map.insert pl b (blocks gs)}



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
instance ToJSON PotentiallyObscured where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON PotentiallyObscured

