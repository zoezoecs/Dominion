
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
module Effects.PlayerIO where

import Polysemy
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import qualified Data.Map as Map

import Types
import Effects.CardEffects
import Effects.Log
import Internal.TH
import Data.Coerce

newtype ReactionEvent card = ReactionEvent {getReactionEvent :: EventAnswer Maybe card} deriving (Eq, Show)
deriving via (EventAnswer Maybe card) instance ToJSON card => ToJSON (ReactionEvent card)
deriving via (EventAnswer Maybe card) instance FromJSON card => FromJSON (ReactionEvent card)

reactionEvent :: forall {k} {card} {m :: k} {a}. CardEffects' card m a -> Maybe a -> ReactionEvent card
reactionEvent ceff ma = ReactionEvent (EventAnswer ceff ma)

instance Functor ReactionEvent where
    fmap f x = coerce $ fmap f (getReactionEvent x)

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetPlayTreasure :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetCardFaceTEMP :: Player -> [CardFace] -> PlayerIO m CardFace
  GetCardTEMP :: Player -> [Card] -> PlayerIO m Card
  GetMCardTEMP :: Player -> [Card] -> PlayerIO m (Maybe Card)
  GetCardsTEMP :: Player -> [Card] -> PlayerIO m [Card]
  GetNCardsTEMP :: Player -> Int -> [Card] -> PlayerIO m [Card]
  GetUpToNCardsTEMP :: Player -> Int -> [Card] -> PlayerIO m [Card]
  SendInfo :: Player -> Log PotentiallyObscured m a -> PlayerIO m () -- Monomorphised card for less GHC extensions
  SendStack :: PlayerPosition -> [Card] -> PlayerIO m ()
  GetPlayerReaction :: Player -> ReactionEvent PotentiallyObscured -> [Card] -> PlayerIO m (Maybe Card)
makeSem ''PlayerIO
deriveJSONGADT ''PlayerIO
deriveArgDict ''PlayerIO
deriving instance Show (PlayerIO m a)

genNoR' (Map.singleton ''Log 'logMapR) ''PlayerIO
playerIOmapR :: PlayerIO m1 a -> PlayerIO m2 a
playerIOmapR = chR_PlayerIO