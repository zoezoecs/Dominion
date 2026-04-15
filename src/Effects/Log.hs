{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Effects.Log where

import Polysemy
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras
import Data.Type.Equality
import Data.GADT.Compare
import Data.Some.Newtype

import Types
import Effects.CardEffects
import Internal.TH

data LoggedEvent card = forall m a. LogEvent (CardEffects' card m a) a

instance Functor LoggedEvent where
  fmap _ (LogEvent (ModifyActions n) x) = LogEvent (ModifyActions n) x
  fmap _ (LogEvent (ModifyBuys n) x) = LogEvent (ModifyBuys n) x
  fmap _ (LogEvent (ModifyCurrency n) x) = LogEvent (ModifyCurrency n) x
  fmap f (LogEvent (ActivateCard pl c) x) = LogEvent (ActivateCard pl (f c)) x
  fmap f (LogEvent (DrawOnce pl) x) = LogEvent (DrawOnce pl) (fmap f x)
  fmap f (LogEvent (BlockOne pl c) x) = LogEvent (BlockOne pl (f c)) x
  fmap f (LogEvent (Discard pl c) x) = LogEvent (Discard pl (f c)) x
  fmap f (LogEvent (TrashCard pl c) x) = LogEvent (TrashCard pl (f c)) x
  fmap f (LogEvent (Reveal pl c) x) = LogEvent (Reveal pl (f c)) x
  fmap f (LogEvent (TopDeck pl c) x) = LogEvent (TopDeck pl (f c)) x
  fmap f (LogEvent (GainCardTo pl cf pp) x) = LogEvent (GainCardTo pl cf pp) (fmap f x)

instance ToJSON card => ToJSON (LoggedEvent card) where
  toJSON (LogEvent eff result) =
    has @ToJSON eff $ object
      [ "effect" .= toJSON eff
      , "result" .= toJSON result
      ]

instance FromJSON card => FromJSON (LoggedEvent card) where
  parseJSON = withObject "LoggedEvent" $ \o -> do
    Some eff <- o .: "effect"
    has @FromJSON eff $ do
      result <- parseJSON =<< o .: "result"
      pure (LogEvent eff result)

instance Eq card => Eq (LoggedEvent card) where
  LogEvent eff1 result1 == LogEvent eff2 result2 =
    case geq eff1 (cardEffectrMap eff2) of
      Nothing   -> False
      Just Refl -> has @Eq eff1 $ result1 == result2

instance Show card => Show (LoggedEvent card) where
  show (LogEvent eff result) =
    has @Show eff $ "LogEvent (" <> show eff <> ") (" <> show result <> ")"



data Log card m a where
  LogPlayerRoundStart :: Player -> Log card m ()
  LogBuy :: Player -> CardFace -> Log card m ()
  LogAct :: Player -> card -> Log card m ()
  LogTreasure :: Player -> card -> Log card m ()
  LogEffect :: LoggedEvent card -> Log card m ()
  -- POLYSEMY ANNOYING: This constructor runs in to a _lot_ of issues with unifying the `a` type variable when we try to map the card variable
  -- to inject into an Either type. We can give up the type level guarantee that the result of an effect matches the actual effect, since we
  -- have to do lots of boilerplate enumeration anyways for everything. If we could make the interpreters have boilerplate free TH free code,
  -- we might be able to come back here and fix this.
  -- LogModifyActions :: Answer Int -> Int -> Log card m ()
  -- LogModifyBuys :: Answer Int -> Int -> Log card m ()
  -- LogModifyCurrency :: Answer Int -> Int -> Log card m ()
  -- LogActivateCard :: Player -> card -> Log card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- LogDrawOnce :: Answer (Maybe card) -> Player -> Log card m ()  -- Note Maybe signals no cards in both draw AND discard
  -- LogBlockOne :: Player -> card -> Log card m () -- Blocks the next attack? This could so lead to a bug lmao...
  -- LogDiscard :: Player -> card -> Log card m () -- NOTE: None of these are "discard FROM HAND" or anything
  -- LogTrashCard :: Player -> card -> Log card m ()
  -- LogReveal :: Player -> card -> Log card m ()
  -- LogTopDeck :: Player -> card -> Log card m ()
  -- LogGainCardTo :: Answer (Either InvalidGain card) -> Player -> CardFace -> PlayerPosition -> Log card m ()
makeSemMonomorphised ''Card ''Log
deriveJSONGADT ''Log

logCardMap :: (c1 -> c2) -> Log c1 m a -> Log c2 m a
logCardMap f (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap f (LogBuy pl cf) = LogBuy pl cf
logCardMap f (LogAct pl c) = LogAct pl (f c)
logCardMap f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap f (LogEffect eff) = LogEffect (fmap f eff)

data LogToPlayer card m a where
  LogToPlayer :: Log card m () -> Player -> LogToPlayer card m ()
makeSem ''LogToPlayer

-- deriving instance (Show a, Show card) => Show (Log card m a)

data Obscure m a where
  GetTempId :: Card -> Obscure m TempId
makeSem ''Obscure

data Correlation m a where
  MkCorrelation :: m a -> Correlation m a
makeSem ''Correlation
