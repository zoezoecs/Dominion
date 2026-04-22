{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
{-# OPTIONS_GHC -w #-}
module Playing.Witness where

import Internal.TH
import Types

data CardEffects''' card m a where
  -- Modify game resources
  BModifyActions :: Int -> CardEffects''' card m Int
  BModifyBuys :: Int -> CardEffects''' card m Int
  BModifyCurrency :: Int -> CardEffects''' card m Int

  BActivateCard :: Player -> card -> CardEffects''' card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  BDrawOnce :: Player -> CardEffects''' card m (Maybe card)  -- Note Maybe signals no cards in both draw AND discard
  BBlockOne :: Player -> card -> CardEffects''' card m () -- Blocks the next attack? This could so lead to a bug lmao...
  BDiscard :: Player -> card -> CardEffects''' card m () -- NOTE: None of these are "discard FROM HAND" or anything
  BTrashCard :: Player -> card -> CardEffects''' card m ()
  BReveal :: Player -> card -> CardEffects''' card m ()
  BTopDeck :: Player -> card -> CardEffects''' card m ()
  BGainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects''' card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffects'''

data CardEffects' card m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects' card m Int
  ModifyBuys :: Int -> CardEffects' card m Int
  ModifyCurrency :: Int -> CardEffects' card m Int

  ActivateCard :: Player -> card -> CardEffects' card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects' card m (Maybe card)  -- Note Maybe signals no cards in both draw AND discard
  BlockOne :: Player -> card -> CardEffects' card m () -- Blocks the next attack? This could so lead to a bug lmao...
  Discard :: Player -> card -> CardEffects' card m () -- NOTE: None of these are "discard FROM HAND" or anything
  TrashCard :: Player -> card -> CardEffects' card m ()
  Reveal :: Player -> card -> CardEffects' card m ()
  TopDeck :: Player -> card -> CardEffects' card m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects' card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffects'

deriving instance (Show a, Show card) => Show (CardEffects' card m a)
type CardEffects = CardEffects' Card


data Loggable card a where
  LogEvent :: Show a => CardEffects' card m a -> Loggable card a
deriving instance Show card => Show (Loggable card a)

data Log card m a where
  LogPlayerRoundStart :: Player -> Log card m ()
  LogBuy :: Player -> CardFace -> Log card m ()
  LogAct :: Show card => Player -> card -> Log card m ()
  LogTreasure :: Show card => Player -> card -> Log card m ()
  LogEffect :: (Show card) => Loggable card a -> a -> Log card m a

newtype Answer a = Ans {getAns :: a} deriving (Eq, Ord, Show, Functor)
data CardEffects'' card where
  -- Modify game resources
  XModifyActions ::  Int  -> Answer Int -> CardEffects'' card
  XModifyBuys ::  Int  -> Answer Int -> CardEffects'' card
  XModifyCurrency ::  Int  -> Answer Int -> CardEffects'' card

  XActivateCard ::  Player -> card  -> Answer () -> CardEffects'' card
  XDrawOnce ::  Player  -> Answer (Maybe card) -> CardEffects'' card
  XBlockOne ::  Player -> card  -> Answer () -> CardEffects'' card 
  XDiscard ::  Player -> card  -> Answer () -> CardEffects'' card
  XTrashCard ::  Player -> card  -> Answer () -> CardEffects'' card
  XReveal ::  Player -> card  -> Answer () -> CardEffects'' card
  XTopDeck ::  Player -> card  -> Answer () -> CardEffects'' card
  XGainCardTo ::  Player -> CardFace -> PlayerPosition  -> Answer (Either InvalidGain card) -> CardEffects'' card
deriving instance Show card => Show (CardEffects'' card)
deriving instance Functor CardEffects''

removeAParameter :: CardEffects' card m a -> a -> CardEffects'' card
removeAParameter ((ModifyActions n)) x = XModifyActions n (Ans x)
removeAParameter ((ModifyBuys n)) x = XModifyBuys n (Ans x)
removeAParameter ((ModifyCurrency n)) x = XModifyCurrency n (Ans x)
removeAParameter ((ActivateCard pl c)) x = XActivateCard pl c (Ans x)
removeAParameter ((DrawOnce pl)) x = XDrawOnce pl (Ans x)
removeAParameter ((BlockOne pl c)) x = XBlockOne pl c (Ans x)
removeAParameter ((Discard pl c)) x = XDiscard pl c (Ans x)
removeAParameter ((TrashCard pl c)) x = XTrashCard pl c (Ans x)
removeAParameter ((Reveal pl c)) x = XReveal pl c (Ans x)
removeAParameter ((TopDeck pl c)) x = XTopDeck pl c (Ans x)
removeAParameter ((GainCardTo pl cf pos)) x = XGainCardTo pl cf pos (Ans x)

addAParameter :: CardEffects'' card -> CardEffects' card m a
-- addAParameter ((XModifyActions n _)) = ModifyActions n
-- addAParameter ((XModifyBuys n _)) = ModifyBuys n
-- addAParameter ((XModifyCurrency n _)) = ModifyCurrency n
-- addAParameter ((XActivateCard pl c _)) = ActivateCard pl c
-- addAParameter ((XDrawOnce pl _)) = DrawOnce pl
-- addAParameter ((XBlockOne pl c _)) = BlockOne pl c
-- addAParameter ((XDiscard pl c _)) = Discard pl c
-- addAParameter ((XTrashCard pl c _)) = TrashCard pl c
-- addAParameter ((XReveal pl c _)) = Reveal pl c
-- addAParameter ((XTopDeck pl c _)) = TopDeck pl c
-- addAParameter ((XGainCardTo pl cf pos _)) = GainCardTo pl cf pos
addAParameter _ = undefined

data CardEffectWitness c1 c2 a b where
  WitnessInt :: CardEffectWitness c1 c2 Int Int
  WitnessUnit :: CardEffectWitness c1 c2 () ()
  WitnessMCard :: CardEffectWitness c1 c2 (Maybe c1) (Maybe c2)
  WitnessEither :: CardEffectWitness c1 c2 (Either x c1) (Either x c2)

cardMap :: (card1 -> card2) -> CardEffects' card1 m a -> CardEffectWitness card1 card2 a b -> CardEffects' card2 m b
cardMap f (ModifyActions n) WitnessInt = ModifyActions n
cardMap f (ModifyBuys n) WitnessInt = ModifyBuys n
cardMap f (ModifyCurrency n) WitnessInt = ModifyCurrency n
cardMap f (ActivateCard pl c) WitnessUnit = ActivateCard pl (f c)
cardMap f (DrawOnce pl) WitnessMCard = DrawOnce pl
cardMap f (BlockOne pl c) WitnessUnit = BlockOne pl (f c)
cardMap f (Discard pl c) WitnessUnit = Discard pl (f c)
cardMap f (TrashCard pl c) WitnessUnit = TrashCard pl (f c)
cardMap f (Reveal pl c) WitnessUnit = Reveal pl (f c)
cardMap f (TopDeck pl c) WitnessUnit = TopDeck pl (f c)
cardMap f (GainCardTo pl cf pp) WitnessEither = GainCardTo pl cf pp

witnessMap :: (card1 -> card2) -> CardEffectWitness card1 card2 a b -> a -> b
witnessMap f WitnessInt n = n
witnessMap f WitnessUnit () = ()
witnessMap f WitnessMCard mc = f <$> mc
witnessMap f WitnessEither ec = f <$> ec

logCardMap :: Show c2 => (c1 -> c2) -> Log c1 m a -> Log c2 m a
logCardMap f (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap f (LogBuy pl cf) = LogBuy pl cf
logCardMap f (LogAct pl c) = LogAct pl (f c)
logCardMap f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap f (LogEffect (LogEvent eff) ans) = undefined

logCardMap' :: (Show c2, Show b) => CardEffectWitness c1 c2 a b -> (c1 -> c2) -> Log c1 m a -> Log c2 m b
logCardMap' WitnessUnit f (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap' WitnessUnit f (LogBuy pl cf) = LogBuy pl cf
logCardMap' WitnessUnit f (LogAct pl c) = LogAct pl (f c)
logCardMap' WitnessUnit f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap' witness f (LogEffect (LogEvent eff) ans) = LogEffect (LogEvent (cardMap f eff witness)) (witnessMap f witness ans)

type family EffectResult (card :: *) where
  EffectResult card = Maybe card

wah :: EffectResult a
wah = undefined
-- POLYSEMY ANNOYING:
-- We have to monomorphise here for a = CardEffects m a to avoid Polysemy thinking Loggable (CardEffects m a) is higher order.
-- We also need this thing to carry around the proof that the output of CardEffects m a is always showable, because we know the constructors.
-- This allows us to have a LogEffect constructor in Log.

-- This is inspecting each constructor to see that there must implicitly be a Show a for each a
-- It looks like its doing nothing, but its actually implicitly packing a Show instance dict
showLoggable :: Show card => CardEffects' card r a -> Loggable card a
showLoggable (ModifyActions n) = LogEvent (ModifyActions n)
showLoggable (ModifyBuys n) = LogEvent (ModifyBuys n)
showLoggable (ModifyCurrency n) = LogEvent (ModifyCurrency n)
showLoggable (ActivateCard pl c) = LogEvent (ActivateCard pl c)
showLoggable (DrawOnce pl) = LogEvent (DrawOnce pl)
showLoggable (BlockOne pl c) = LogEvent (BlockOne pl c)
showLoggable (Discard pl c) = LogEvent (Discard pl c)
showLoggable (TrashCard pl c) = LogEvent (TrashCard pl c)
showLoggable (Reveal pl c) = LogEvent (Reveal pl c)
showLoggable (TopDeck pl c) = LogEvent (TopDeck pl c)
showLoggable (GainCardTo pl c pos) = LogEvent (GainCardTo pl c pos)

-- data FOCardEffects a = MkFOCE {getFOCE :: forall k (m :: k). CardEffects m a}
-- toFOCE :: CardEffects m a -> FOCardEffects a
-- toFOCE x = MkFOCE $ cardEffectrMap x
-- 
-- fromFOCE :: FOCardEffects a -> CardEffects m a
-- fromFOCE (MkFOCE x) = x