{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Effects.CardEffects where

import Polysemy
import Control.Monad
import Data.Maybe
import Data.Aeson.GADT.TH
import Data.Constraint.Extras
import Data.Type.Equality
import Data.GADT.Compare
import Data.Functor.Classes
import Data.Traversable
import Data.Aeson

import Data.Some.Newtype

import Types
import Internal.TH


data CardEffects' card m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects' card m Int
  ModifyBuys :: Int -> CardEffects' card m Int
  ModifyCurrency :: Int -> CardEffects' card m Int

  ActivateCard :: Player -> card -> CardEffects' card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects' card m (Maybe card)  -- Note Maybe signals no cards in both draw AND discard
  BlockOne :: Player -> card -> CardEffects' card m () -- Blocks the next attack
  Discard :: Player -> card -> CardEffects' card m () -- NOTE: None of these are "discard FROM HAND" or anything
  TrashCard :: Player -> card -> CardEffects' card m ()
  Reveal :: Player -> card -> CardEffects' card m ()
  TopDeck :: Player -> card -> CardEffects' card m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects' card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffects'
deriving instance (Show a, Show card) => Show (CardEffects' card m a)
deriving instance (Eq card) => Eq (CardEffects' card m a)
type CardEffects = CardEffects' Card

data EventAnswer f card = forall m a. EventAnswer (CardEffects' card m a) (f a)

getEffectPlayer :: CardEffects' card m a -> Maybe Player
getEffectPlayer (ModifyActions _) = Nothing
getEffectPlayer (ModifyBuys _) = Nothing
getEffectPlayer (ModifyCurrency _) = Nothing
getEffectPlayer (ActivateCard pl _) = Just pl
getEffectPlayer (DrawOnce pl) = Just pl
getEffectPlayer (BlockOne pl _) = Just pl
getEffectPlayer (Discard pl _) = Just pl
getEffectPlayer (TrashCard pl _) = Just pl
getEffectPlayer (Reveal pl _) = Just pl
getEffectPlayer (TopDeck pl _) = Just pl
getEffectPlayer (GainCardTo pl _ _) = Just pl


traverse'' :: (Applicative f, Traversable f1) => (c1 -> f c2) -> EventAnswer f1 c1 -> f (EventAnswer f1 c2)
traverse'' _ (EventAnswer (ModifyActions n) x) = pure $ EventAnswer (ModifyActions n) x
traverse'' _ (EventAnswer (ModifyBuys n) x) = pure $ EventAnswer (ModifyBuys n) x
traverse'' _ (EventAnswer (ModifyCurrency n) x) = pure $ EventAnswer (ModifyCurrency n) x
traverse'' f (EventAnswer (ActivateCard pl c) x) = fmap (\a -> EventAnswer (ActivateCard pl a) x) (f c)
traverse'' f (EventAnswer (DrawOnce pl) x) = fmap (EventAnswer (DrawOnce pl)) (traverse (traverse f) x)
traverse'' f (EventAnswer (BlockOne pl c) x) = fmap (\a -> EventAnswer (BlockOne pl a) x) (f c)
traverse'' f (EventAnswer (Discard pl c) x) = fmap (\a -> EventAnswer (Discard pl a) x) (f c)
traverse'' f (EventAnswer (TrashCard pl c) x) = fmap (\a -> EventAnswer (TrashCard pl a) x) (f c)
traverse'' f (EventAnswer (Reveal pl c) x) = fmap (\a -> EventAnswer (Reveal pl a) x) (f c)
traverse'' f (EventAnswer (TopDeck pl c) x) = fmap (\a -> EventAnswer (TopDeck pl a) x) (f c)
traverse'' f (EventAnswer (GainCardTo pl cf pos) x) = fmap (EventAnswer (GainCardTo pl cf pos)) (traverse (traverse f) x)

instance (Traversable f1) => Traversable (EventAnswer f1) where
    traverse = traverse''

instance (Traversable f1) => Functor (EventAnswer f1) where
    fmap = fmapDefault

instance (Traversable f1) => Foldable (EventAnswer f1) where
    foldMap = foldMapDefault

genNoR ''CardEffects'

cardEffectrMap :: CardEffects' card r1 a -> CardEffects' card r2 a
cardEffectrMap = chR_CardEffects'

deriveJSONGADT ''CardEffects'

-- constraints-extras-0.4.0.2 deriveArgDict does not derive this because it does not correctly use the 
-- same type variables for instances of card in the constraints and the rest of signature
instance (c Int, c (), c (Maybe card), c (Either InvalidGain card)) 
    => Has c (CardEffects' card m) where
  has eff k = case eff of
    ModifyActions{}  -> k
    ModifyBuys{}     -> k
    ModifyCurrency{} -> k
    DrawOnce{}       -> k
    GainCardTo{}     -> k
    ActivateCard{}   -> k
    BlockOne{}       -> k
    Discard{}        -> k
    TrashCard{}      -> k
    Reveal{}         -> k
    TopDeck{}        -> k

-- dependent-sum-template-0.2.0.1 does not derive this because it cannot derive the Eq card constraint
instance Eq card => GEq (CardEffects' card m) where
  geq (ModifyActions n1) (ModifyActions n2) = if n1 == n2 then Just Refl else Nothing
  geq (ModifyBuys n1) (ModifyBuys n2) = if n1 == n2 then Just Refl else Nothing
  geq (ModifyCurrency n1) (ModifyCurrency n2) = if n1 == n2 then Just Refl else Nothing
  geq (ActivateCard p1 c1) (ActivateCard p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (DrawOnce p1) (DrawOnce p2) = if p1 == p2 then Just Refl else Nothing
  geq (BlockOne p1 c1) (BlockOne p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (Discard p1 c1) (Discard p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (TrashCard p1 c1) (TrashCard p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (Reveal p1 c1) (Reveal p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (TopDeck p1 c1) (TopDeck p2 c2) = if p1 == p2 && c1 == c2 then Just Refl else Nothing
  geq (GainCardTo p1 f1 pos1) (GainCardTo p2 f2 pos2) = if p1 == p2 && f1 == f2 && pos1 == pos2 then Just Refl else Nothing
  geq _ _ = Nothing

instance (Eq1 f, Eq a) => Eq (EventAnswer f a) where
  (EventAnswer ceff1 fa1) == (EventAnswer ceff2 fa2) = case geq ceff1 (cardEffectrMap ceff2) of
      Nothing   -> False
      Just Refl -> has @Eq ceff1 $ fa1 == fa2

instance (Show1 f, Show a) => Show (EventAnswer f a) where
  show (EventAnswer eff result) = has @Show eff $ "EventAnswer (" <> show eff <> ") (" <> show result <> ")"

instance (ToJSON1 f, ToJSON card) => ToJSON (EventAnswer f card) where
  toJSON (EventAnswer eff result) =
    has @ToJSON eff $ object
      [ "effect" .= toJSON eff
      , "result" .= toJSON1 result
      ]

instance (FromJSON1 f, FromJSON card) => FromJSON (EventAnswer f card) where
  parseJSON = withObject "LoggedEvent" $ \o -> do
    Some eff <- o .: "effect"
    has @FromJSON eff $ do
      result <- parseJSON1 =<< o .: "result"
      pure (EventAnswer eff result)

drawCard :: Member CardEffects r => Player -> Int -> Sem r [Card]
drawCard player n = fmap catMaybes $ replicateM n $ drawOnce player

gainCard :: Member CardEffects r => Player -> CardFace -> Sem r (Either InvalidGain Card)
gainCard pl cf = gainCardTo pl cf PlayerDiscardPile
