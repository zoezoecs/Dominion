
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Effects.PlayerIO where

import Polysemy
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Type.Equality
import Data.GADT.Compare
import Data.Some.Newtype
import qualified Data.Map as Map

import Types
import Effects.CardEffects
import Effects.Log
import Internal.TH
import Data.Coerce

-- TODO: Coerce and newtypes?
evAnsReaction :: EventAnswer Maybe card -> ReactionEvent card
evAnsReaction (EventAnswer e x) = ReactionEvent (EventAnswer e x)

reactionEvAns :: ReactionEvent card -> EventAnswer Maybe card
reactionEvAns (ReactionEvent (EventAnswer e x)) = EventAnswer e x

newtype ReactionEvent card = ReactionEvent {getReactionEvent :: EventAnswer Maybe card}

reactionEvent :: forall {k} {card} {m :: k} {a}. CardEffects' card m a -> Maybe a -> ReactionEvent card
reactionEvent ceff ma = ReactionEvent (EventAnswer ceff ma)

instance Functor ReactionEvent where
    fmap f x = coerce $ fmap f (getReactionEvent x)

instance ToJSON card => ToJSON (ReactionEvent card) where
  toJSON (ReactionEvent (EventAnswer eff result)) =
    has @ToJSON eff $ object
      [ "effect" .= toJSON eff
      , "result" .= toJSON result
      ]

instance FromJSON card => FromJSON (ReactionEvent card) where
  parseJSON = withObject "LoggedEvent" $ \o -> do
    Some eff <- o .: "effect"
    has @FromJSON eff $ do
      result <- parseJSON =<< o .: "result"
      pure (ReactionEvent (EventAnswer eff result))

instance Eq card => Eq (ReactionEvent card) where
  ReactionEvent (EventAnswer eff1 result1) == ReactionEvent (EventAnswer eff2 result2) = 
    case geq eff1 (cardEffectrMap eff2) of
      Nothing   -> False
      Just Refl -> has @Eq eff1 $ result1 == result2

instance Show card => Show (ReactionEvent card) where
  show (ReactionEvent (EventAnswer eff result)) =
    has @Show eff $ "ReactionEvent (" <> show eff <> ") (" <> show result <> ")"

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetPlayTreasure :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetCardFaceTEMP :: Player -> [CardFace] -> PlayerIO m CardFace
  GetCardTEMP :: Player -> [Card] -> PlayerIO m Card
  GetMCardTEMP :: Player -> [Card] -> PlayerIO m (Maybe Card) -- Tbh these should not have [Card] args anyways
  GetCardsTEMP :: Player -> [Card] -> PlayerIO m [Card]
  SendInfo :: Player -> Log PotentiallyObscured m a -> PlayerIO m () -- Monomorphised card for less GHC extensions
  SendStack :: PlayerPosition -> [Card] -> PlayerIO m ()
  GetPlayerReaction :: Player -> ReactionEvent PotentiallyObscured -> PlayerIO m (Maybe Card)
makeSem ''PlayerIO
deriveJSONGADT ''PlayerIO
deriveArgDict ''PlayerIO
deriving instance Show (PlayerIO m a)

genNoR' (Map.singleton ''Log 'logMapR) ''PlayerIO
playerIOmapR :: PlayerIO m1 a -> PlayerIO m2 a
playerIOmapR = chR_PlayerIO

getPlayerReaction' :: Member PlayerIO r => Player -> (forall m. CardEffects' PotentiallyObscured m a) -> Maybe a -> Sem r (Maybe Card)
getPlayerReaction' pl ceff ma = getPlayerReaction pl (reactionEvent ceff ma)