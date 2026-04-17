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
import Data.Functor.Identity
import Data.Functor.Const

import Types
import Effects.CardEffects
import Internal.TH

data EventAnswer f card = forall m a. EventAnswer (CardEffects' card m a) (f a)

traverse'' :: (Applicative f, Traversable f1) => (c1 -> f c2) -> EventAnswer f1 c1 -> f (EventAnswer f1 c2)
traverse'' f (EventAnswer (ModifyActions n) x) = pure $ EventAnswer (ModifyActions n) x
traverse'' f (EventAnswer (ModifyBuys n) x) = pure $ EventAnswer (ModifyBuys n) x
traverse'' f (EventAnswer (ModifyCurrency n) x) = pure $ EventAnswer (ModifyCurrency n) x
traverse'' f (EventAnswer (ActivateCard pl c) x) = fmap (\a -> EventAnswer (ActivateCard pl a) x) (f c)
traverse'' f (EventAnswer (DrawOnce pl) x) = fmap (EventAnswer (DrawOnce pl)) (traverse (traverse f) x)
traverse'' f (EventAnswer (BlockOne pl c) x) = fmap (\a -> EventAnswer (BlockOne pl a) x) (f c)
traverse'' f (EventAnswer (Discard pl c) x) = fmap (\a -> EventAnswer (Discard pl a) x) (f c)
traverse'' f (EventAnswer (TrashCard pl c) x) = fmap (\a -> EventAnswer (TrashCard pl a) x) (f c)
traverse'' f (EventAnswer (Reveal pl c) x) = fmap (\a -> EventAnswer (Reveal pl a) x) (f c)
traverse'' f (EventAnswer (TopDeck pl c) x) = fmap (\a -> EventAnswer (TopDeck pl a) x) (f c)
traverse'' f (EventAnswer (GainCardTo pl cf pos) x) = fmap (EventAnswer (GainCardTo pl cf pos)) (traverse (traverse f) x)

instance (Traversable f1) => Functor (EventAnswer f1) where
    fmap f = runIdentity . traverse (Identity . f)

instance (Traversable f1) => Foldable (EventAnswer f1) where
    foldMap f = getConst . traverse (Const . f)

instance (Traversable f1) => Traversable (EventAnswer f1) where
    traverse = traverse''

data LoggedEvent card = forall m a. LogEvent (CardEffects' card m a) a

logEvAnswer :: EventAnswer Identity card -> LoggedEvent card
logEvAnswer (EventAnswer eff (Identity x)) = LogEvent eff x

evAnswerLog :: LoggedEvent card -> EventAnswer Identity card
evAnswerLog (LogEvent eff x) = EventAnswer eff (Identity x)

traverse' :: Applicative f => (c1 -> f c2) -> LoggedEvent c1 -> f (LoggedEvent c2)
traverse' f x = logEvAnswer <$> traverse'' f (evAnswerLog x)

instance Functor LoggedEvent where
    fmap f = runIdentity . traverse (Identity . f)

instance Foldable LoggedEvent where
    foldMap f = getConst . traverse (Const . f)

instance Traversable LoggedEvent where
    traverse = traverse'

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
