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

logToPlayerLog :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead, Obscure] r) => Sem (Log Card : r) a -> Sem r a
logToPlayerLog = interpret $ \case
  LogPlayerRoundStart player -> logAll0 (LogPlayerRoundStart player)
  LogBuy player cf -> logAll0 (LogBuy player cf)
  LogAct player card -> logAll (LogAct player) card
  LogTreasure player card -> logAll (LogTreasure player) card
  LogEffect a@(LogEvent (ModifyActions {}) _) ->  logEffectAll a
  LogEffect a@(LogEvent (ModifyBuys {}) _) ->     logEffectAll a
  LogEffect a@(LogEvent (ModifyCurrency {}) _) -> logEffectAll a
  LogEffect a@(LogEvent (ActivateCard _ _) ()) -> logEffectAll a
  LogEffect a@(LogEvent (DrawOnce pl) _) ->       logRedactedEff pl a
  LogEffect a@(LogEvent (BlockOne _ _) ()) ->     logEffectAll a
  LogEffect a@(LogEvent (Discard pl _) ()) ->     logRedactedEff pl a
  LogEffect a@(LogEvent (TrashCard pl _) ()) ->   logRedactedEff pl a
  LogEffect a@(LogEvent (Reveal _ _) ()) ->       logEffectAll a
  LogEffect a@(LogEvent (TopDeck pl _) ()) ->     logRedactedEff pl a
  LogEffect a@(LogEvent (GainCardTo pl _ _) _) -> logRedactedEff pl a
  where
    dontRedact :: Member Obscure r => Card -> Sem r PotentiallyObscured
    dontRedact card = fmap (\tid -> PObscured $ Left (card,tid)) (getTempId card)

    redactEff :: Member Obscure r => Card -> Sem r PotentiallyObscured
    redactEff = fmap (PObscured . Right . Obscured) . getTempId

    -- We can't write a Traversable instance for Log card m a due to the a being out. Its easier to just manually pipe the cards through here
    -- than it would be to make another existential type, which wouldn't even work properly with the effects system
    -- So the logging for the non effects is slightly different to logging the LogEffect cardeffect, for which we have a traversable instance.
    -- That is why there is a 0 and a 1 version of logAll - for the number of card occurrences to not redact
    logAll0 :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead] r) => (forall card. Log card (Sem r) ()) -> Sem r ()
    logAll0 = void . applyToAll . logToPlayer

    logAll :: (Members '[LogToPlayer PotentiallyObscured, Obscure, BoardStateRead] r) => (forall card. card -> Log card (Sem r) ()) -> Card -> Sem r ()
    logAll f x = void $ applyToAll . logToPlayer . f <$> dontRedact x

    logEffectAll :: (Members '[LogToPlayer PotentiallyObscured, Obscure, BoardStateRead] r) => LoggedEvent Card -> Sem r ()
    logEffectAll eff = void $ do
      public <- traverse dontRedact eff
      applyToAll $ logToPlayer (LogEffect public)

    logRedactedEff :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead, Obscure] r) =>
                   Player ->
                   LoggedEvent Card ->
                   Sem r ()
    logRedactedEff pl eff = do
      secret <- traverse dontRedact eff
      public <- traverse redactEff eff
      _ <- logToPlayer (LogEffect secret) pl
      _ <- applyToOthers pl (logToPlayer (LogEffect public))
      return ()

logPlayerToPlayerIO :: Member PlayerIO r => Sem (LogToPlayer PotentiallyObscured : r) a -> Sem r a
logPlayerToPlayerIO = transform @_ @PlayerIO (\(LogToPlayer eff pl) -> SendInfo pl eff)

logPlayerToString :: (ToJSON card, Member (Output (Player, LazyByteString)) r) => Sem (LogToPlayer card : r) a -> Sem r a
logPlayerToString = interpret (\(LogToPlayer eff pl) -> output (pl, encode eff))

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
