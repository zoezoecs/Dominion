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

cardEffectrMap :: CardEffects' card r1 a -> CardEffects' card r2 a
cardEffectrMap (ModifyActions n) = ModifyActions n
cardEffectrMap (ModifyBuys n) = ModifyBuys n
cardEffectrMap (ModifyCurrency n) = ModifyCurrency n

cardEffectrMap (ActivateCard pl c) = ActivateCard pl c
cardEffectrMap (DrawOnce pl) = DrawOnce pl
cardEffectrMap (BlockOne pl c) = BlockOne pl c
cardEffectrMap (Discard pl c) = Discard pl c
cardEffectrMap (TrashCard pl c) = TrashCard pl c
cardEffectrMap (Reveal pl c) = Reveal pl c
cardEffectrMap (TopDeck pl c) = TopDeck pl c
cardEffectrMap (GainCardTo pl cf pp) = GainCardTo pl cf pp

deriveJSONGADT ''CardEffects'
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


drawCard :: Member CardEffects r => Player -> Int -> Sem r [Card]
drawCard player n = fmap catMaybes $ replicateM n $ drawOnce player

gainCard :: Member CardEffects r => Player -> CardFace -> Sem r (Either InvalidGain Card)
gainCard pl cf = gainCardTo pl cf PlayerDiscardPile
