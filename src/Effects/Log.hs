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


--data LoggedEvent card = forall m a. LoggedEvent (CardEffects' card m a) a
newtype LoggedEvent card = LoggedEvent {getLoggedEvent :: EventAnswer Identity card}

loggedEvent :: forall {k} {card} {m :: k} {a}. CardEffects' card m a -> a -> LoggedEvent card
loggedEvent ceff a = LoggedEvent (EventAnswer ceff (Identity a))

logEvAnswer :: EventAnswer Identity card -> LoggedEvent card
logEvAnswer (EventAnswer eff (Identity x)) = loggedEvent eff x

evAnswerLog :: LoggedEvent card -> EventAnswer Identity card
evAnswerLog (LoggedEvent (EventAnswer eff x)) = EventAnswer eff x

traverse' :: Applicative f => (c1 -> f c2) -> LoggedEvent c1 -> f (LoggedEvent c2)
traverse' f x = logEvAnswer <$> traverse'' f (evAnswerLog x)

instance Functor LoggedEvent where
    fmap f = runIdentity . traverse (Identity . f)

instance Foldable LoggedEvent where
    foldMap f = getConst . traverse (Const . f)

instance Traversable LoggedEvent where
    traverse = traverse'

instance ToJSON card => ToJSON (LoggedEvent card) where
  toJSON (LoggedEvent (EventAnswer eff result)) =
    has @ToJSON eff $ object
      [ "effect" .= toJSON eff
      , "result" .= toJSON result
      ]

instance FromJSON card => FromJSON (LoggedEvent card) where
  parseJSON = withObject "LoggedEvent" $ \o -> do
    Some eff <- o .: "effect"
    has @FromJSON eff $ do
      result <- parseJSON =<< o .: "result"
      pure (loggedEvent eff result)

instance Eq card => Eq (LoggedEvent card) where
  LoggedEvent (EventAnswer eff1 result1) == LoggedEvent (EventAnswer eff2 result2) = 
    case geq eff1 (cardEffectrMap eff2) of
      Nothing   -> False
      Just Refl -> has @Eq eff1 $ result1 == result2

instance Show card => Show (LoggedEvent card) where
  show (LoggedEvent (EventAnswer eff result)) =
    has @Show eff $ "LoggedEvent (" <> show eff <> ") (" <> show result <> ")"


data Log card m a where
  LogPlayerRoundStart :: Player -> Log card m ()
  LogBuy :: Player -> CardFace -> Log card m ()
  LogAct :: Player -> card -> Log card m ()
  LogTreasure :: Player -> card -> Log card m ()
  LogEffect :: LoggedEvent card -> Log card m ()
makeSemMonomorphised ''Card ''Log
deriveJSONGADT ''Log
deriving instance Show card => Show (Log card m a)

logCardMap :: (c1 -> c2) -> Log c1 m a -> Log c2 m a
logCardMap f (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap f (LogBuy pl cf) = LogBuy pl cf
logCardMap f (LogAct pl c) = LogAct pl (f c)
logCardMap f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap f (LogEffect eff) = LogEffect (fmap f eff)

genNoR ''Log
logMapR :: Log card m1 a -> Log card m2 a
logMapR = chR_Log

data LogToPlayer card m a where
  LogToPlayer :: Log card m () -> Player -> LogToPlayer card m ()
makeSem ''LogToPlayer
deriving instance Show card => Show (LogToPlayer card m a)

data Obscure m a where
  GetTempId :: Card -> Obscure m TempId
makeSem ''Obscure

data Correlation m a where
  MkCorrelation :: m a -> Correlation m a
makeSem ''Correlation
