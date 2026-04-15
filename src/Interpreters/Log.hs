{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Interpreters.Log where

import Polysemy
import Polysemy.Output
import Polysemy.Scoped
import Polysemy.State

import Data.Aeson
import Data.ByteString.Lazy
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Types
import Effects

effectPipe :: (Member (Log Card) r, Member CardEffects r) => Sem r b -> CardEffects' Card m b -> Sem r b
effectPipe a b = a >>=/ (logEffect . LogEvent b)

logEffects :: Members '[CardEffects, Log Card] r => Sem r a -> Sem r a
logEffects = intercept (\cardEff -> effectPipe (send (cardEffectrMap cardEff)) cardEff)
--logEffects :: Members '[CardEffects, Log Card] r => Sem r a -> Sem r a
--logEffects = intercept $ \case
--  ModifyActions n -> effectPipe (modifyActions n) (ModifyActions n)
--  ModifyBuys n -> effectPipe (modifyBuys n) (ModifyBuys n)
--  ModifyCurrency n -> effectPipe (modifyCurrency n) (ModifyCurrency n)
--  ActivateCard pl c -> effectPipe (activateCard pl c) (ActivateCard pl c)
--  DrawOnce pl -> effectPipe (drawOnce pl) (DrawOnce pl)
--  BlockOne pl c -> effectPipe (blockOne pl c) (BlockOne pl c)
--  Discard pl c -> effectPipe (discard pl c) (Discard pl c)
--  TrashCard pl c -> effectPipe (trashCard pl c) (TrashCard pl c)
--  Reveal pl c -> effectPipe (reveal pl c) (Reveal pl c)
--  TopDeck pl c -> effectPipe (topDeck pl c) (TopDeck pl c)
--  GainCardTo pl c pos -> effectPipe (gainCardTo pl c pos) (GainCardTo pl c pos)

logTurn :: Members '[GameLoop, Log Card] r => Sem r a -> Sem r a
logTurn = intercept $ \case
    StartingResources pl -> startingResources pl
    BuyCard pl c -> ifSuccess (buyCard pl c) (logBuy pl c)
    PlayFromHand pl c -> ifSuccess (playFromHand pl c) (logAct pl c)
    PlayTreasure pl c -> ifSuccess (playTreasure pl c) (logTreasure pl c)
    DrawTurnStart pl n -> logPlayerRoundStart pl >> drawTurnStart pl n
    DiscardHandCleanup pl -> discardHandCleanup pl

-- TODO: This sucks is unreadable and has shit ergonomics
logToPlayerLog :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead, Obscure] r) => Sem (Log Card : r) a -> Sem r a
logToPlayerLog = interpret $ \case
  LogPlayerRoundStart player -> logAll0 (LogPlayerRoundStart player)
  LogBuy player cf -> logAll0 (LogBuy player cf)
  LogAct player card -> logAll (LogAct player) card
  LogTreasure player card -> logAll (LogTreasure player) card
  LogEffect (LogEvent (ModifyActions n) m) -> logEffectAll0 (LogEvent (ModifyActions n) (m))
  LogEffect (LogEvent (ModifyBuys n) m) -> logEffectAll0 (LogEvent (ModifyBuys n) (m))
  LogEffect (LogEvent (ModifyCurrency n) m) -> logEffectAll0 (LogEvent (ModifyCurrency n) (m))
  LogEffect (LogEvent (ActivateCard pl c) ()) -> logEffectAll (\cx -> LogEvent (ActivateCard pl cx) (())) c
  LogEffect (LogEvent (DrawOnce pl) mc) -> logRedactedEff' pl (LogEvent (DrawOnce pl)) (traverse dontRedact mc) (traverse redactEff mc)
  LogEffect (LogEvent (BlockOne pl c) ()) -> logEffectAll (\cx -> LogEvent (BlockOne pl cx) ()) c
  LogEffect (LogEvent (Discard pl c) ()) -> logRedactedEff pl ((\x -> LogEvent (Discard pl x) ()) <$> dontRedact c) ((\x -> LogEvent (Discard pl x) ()) <$> redactEff c)
  LogEffect (LogEvent (TrashCard pl c) ()) -> logRedactedEff pl ((\x -> LogEvent (TrashCard pl x) ()) <$> dontRedact c) ((\x -> LogEvent (TrashCard pl x) ()) <$> redactEff c)
  LogEffect (LogEvent (Reveal pl c) ()) -> logEffectAll (\cx -> LogEvent (Reveal pl cx) ()) c
  LogEffect (LogEvent (TopDeck pl c) ()) -> logRedactedEff pl ((\x -> LogEvent (TopDeck pl x) ()) <$> dontRedact c) ((\x -> LogEvent (TopDeck pl x) ()) <$> redactEff c)
  LogEffect (LogEvent (GainCardTo pl cf pos) mcard) -> logRedactedEff' pl (LogEvent (GainCardTo pl cf pos)) (traverse dontRedact mcard) (traverse redactEff mcard)
  where
    logAll0 :: (Members '[LogToPlayer PotentiallyObscured, Obscure, BoardStateRead] r) => (forall card. Log card (Sem r) ()) -> Sem r ()
    logAll0 x = void $ applyToAll (logToPlayer @PotentiallyObscured (logCardMap Left x))

    logAll :: (Members '[LogToPlayer PotentiallyObscured, Obscure, BoardStateRead] r) => (Card -> Log Card (Sem r) ()) -> Card -> Sem r ()
    logAll f x = do
      tid <- getTempId x
      void $ applyToAll (logToPlayer @PotentiallyObscured (logCardMap (\y -> Left (y, tid)) (f x)))

    logEffectAll0 :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead] r) => (forall card. LoggedEvent card) -> Sem r ()
    logEffectAll0 a = void $ applyToAll $ logToPlayer . logCardMap Left $ LogEffect a

    logEffectAll :: (Members '[LogToPlayer PotentiallyObscured, Obscure, BoardStateRead] r) => (Card -> LoggedEvent Card) -> Card -> Sem r ()
    logEffectAll f a = do
      tid <- getTempId a
      void $ applyToAll $ logToPlayer . logCardMap (\y -> Left (y, tid)) $ LogEffect (f a)

    dontRedact :: Member Obscure r => Card -> Sem r PotentiallyObscured
    dontRedact card = fmap (\tid -> Left (card,tid)) (getTempId card)

    redactEff :: Member Obscure r => Card -> Sem r PotentiallyObscured
    redactEff = fmap (Right . Obscured) . getTempId

    logRedactedEff' :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead] r) =>
                   Player ->
                   (x -> LoggedEvent PotentiallyObscured) ->
                   Sem r x ->
                   Sem r x ->
                   Sem r ()
    logRedactedEff' pl effect_template secret_ans public_ans = do
      gottenSecretAns <- secret_ans
      gottenPublicAns <- public_ans
      _ <- logToPlayer (LogEffect . effect_template  $ gottenSecretAns) pl
      _ <- applyToOthers pl (logToPlayer (LogEffect . effect_template $ gottenPublicAns))
      return ()

    logRedactedEff :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead] r) =>
                   Player ->
                   Sem r (LoggedEvent PotentiallyObscured) ->
                   Sem r (LoggedEvent PotentiallyObscured) ->
                   Sem r ()
    logRedactedEff pl secret_sem public_sem = do
      secret_eff <- secret_sem
      public_eff <- public_sem
      _ <- logToPlayer (LogEffect secret_eff) pl
      _ <- applyToOthers pl (logToPlayer (LogEffect public_eff))
      return ()

logToString :: ToJSON card => Log card m a -> LazyByteString
logToString = encode

logPlayerToString :: (ToJSON card, Member (Output (Player, LazyByteString)) r) => Sem (LogToPlayer card : r) a -> Sem r a
logPlayerToString = interpret (\(LogToPlayer eff pl) -> output (pl, logToString eff))

runObscure :: Member RandomUniqueId r => Sem (Obscure : r) a -> Sem (State (Map Card TempId) : r) a
runObscure = reinterpret $ \case
  GetTempId card -> do
    usedCards <- get
    case Map.lookup card usedCards of
      Just wah -> return wah
      Nothing -> do
        newId <- randomUniqueId
        put $ Map.insert card (MkTempId newId) usedCards
        return (MkTempId newId)

mockState :: s -> Sem (State s : r) a -> Sem r a
mockState s = interpret $ \case
  Get -> return s
  Put _ -> return ()

runCorrelation :: Member RandomUniqueId r => Sem (Scoped_ Obscure ': (Obscure ': r)) a -> Sem r a
runCorrelation = mockState mempty .
                 runObscure .
                 runScopedNew @() (const $ evalState mempty. subsume_ . runObscure)
