
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

import Types
import Effects.CardEffects
import Effects.Log
import Internal.TH
import Data.Coerce

-- TODO: Coerce and newtypes?
evAnsReaction :: EventAnswer Maybe card -> ReactionEvent card
evAnsReaction (EventAnswer e x) = ReactionEvent e x

reactionEvAns :: ReactionEvent card -> EventAnswer Maybe card
reactionEvAns (ReactionEvent e x) = EventAnswer e x

data ReactionEvent card = forall m a. ReactionEvent (CardEffects' card m a) (Maybe a)

instance Functor ReactionEvent where
    fmap f x = evAnsReaction $ fmap f (reactionEvAns x)

instance ToJSON card => ToJSON (ReactionEvent card) where
  toJSON (ReactionEvent eff result) =
    has @ToJSON eff $ object
      [ "effect" .= toJSON eff
      , "result" .= toJSON result
      ]

instance FromJSON card => FromJSON (ReactionEvent card) where
  parseJSON = withObject "LoggedEvent" $ \o -> do
    Some eff <- o .: "effect"
    has @FromJSON eff $ do
      result <- parseJSON =<< o .: "result"
      pure (ReactionEvent eff result)

instance Eq card => Eq (ReactionEvent card) where
  ReactionEvent eff1 result1 == ReactionEvent eff2 result2 = 
    case geq eff1 (cardEffectrMap eff2) of
      Nothing   -> False
      Just Refl -> has @Eq eff1 $ result1 == result2

instance Show card => Show (ReactionEvent card) where
  show (ReactionEvent eff result) =
    has @Show eff $ "ReactionEvent (" <> show eff <> ") (" <> show result <> ")"

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetPlayTreasure :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetTrashAny :: Player -> [Card] -> PlayerIO m [Card]
  GetTrashExactlyN :: Player -> Int -> [Card] -> PlayerIO m [Card]
  SendInfo :: Player -> Log PotentiallyObscured m a -> PlayerIO m () -- Monomorphised card for less GHC extensions
  GetPlayerReaction :: Player -> ReactionEvent PotentiallyObscured -> PlayerIO m (Maybe Card)   -- Where are we redacting the information in ma?
makeSem ''PlayerIO
deriveJSONGADT ''PlayerIO
deriveArgDict ''PlayerIO

getPlayerReaction' :: Member PlayerIO r => Player -> (forall m. CardEffects' PotentiallyObscured m a) -> Maybe a -> Sem r (Maybe Card)
getPlayerReaction' pl ceff ma = getPlayerReaction pl (ReactionEvent ceff ma)