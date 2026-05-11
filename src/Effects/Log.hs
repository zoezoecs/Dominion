{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
module Effects.Log where

import Data.Aeson.GADT.TH
import Data.Aeson
-- import Data.Constraint.Extras
-- import Data.Type.Equality
-- import Data.GADT.Compare
-- import Data.Some.Newtype
import Data.Functor.Identity

import Types
import Effects.CardEffects
import Internal.TH
import Data.Coerce
import Data.Traversable

--data LoggedEvent card = forall m a. LoggedEvent (CardEffects' card m a) a
newtype LoggedEvent card = LoggedEvent {getLoggedEvent :: EventAnswer Identity card} deriving (Eq, Show)
deriving via (EventAnswer Identity card) instance ToJSON card => ToJSON (LoggedEvent card)
deriving via (EventAnswer Identity card) instance FromJSON card => FromJSON (LoggedEvent card)

loggedEvent :: forall {k} {card} {m :: k} {a}. CardEffects' card m a -> a -> LoggedEvent card
loggedEvent ceff a = LoggedEvent (EventAnswer ceff (Identity a))

logEvAnswer :: EventAnswer Identity card -> LoggedEvent card
logEvAnswer = coerce

evAnswerLog :: LoggedEvent card -> EventAnswer Identity card
evAnswerLog = coerce

traverse' :: Applicative f => (c1 -> f c2) -> LoggedEvent c1 -> f (LoggedEvent c2)
traverse' f x = logEvAnswer <$> traverse'' f (evAnswerLog x)

instance Traversable LoggedEvent where
    traverse = traverse'

instance Functor LoggedEvent where
    fmap = fmapDefault

instance Foldable LoggedEvent where
    foldMap = foldMapDefault

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
logCardMap _ (LogPlayerRoundStart pl) = LogPlayerRoundStart pl
logCardMap _ (LogBuy pl cf) = LogBuy pl cf
logCardMap f (LogAct pl c) = LogAct pl (f c)
logCardMap f (LogTreasure pl c) = LogTreasure pl (f c)
logCardMap f (LogEffect eff) = LogEffect (fmap f eff)

genNoR ''Log
logMapR :: Log card m1 a -> Log card m2 a
logMapR = chR_Log
