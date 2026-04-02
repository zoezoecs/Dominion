{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Interpreters.Log where

import Polysemy
import Polysemy.Output

import Control.Monad
import Data.Map (Map)

import Base
import Types
import Effects

effectPipe :: (Member (Log Card) r, Member CardEffects r) => Sem r b -> (Answer b -> CardEffects'' Card) -> Sem r b
effectPipe a b = a >>=/ (logEffect . b . Ans)

-- TODO: Fix boilerplate. Its annoying I can't even use @ patterns...
-- TODO: This should be a lot more granular, since not every player gets every event at the same level of knowledge.
logEffects :: Members '[CardEffects, Log Card] r => Sem (CardEffects : r) a -> Sem r a
logEffects = interpret $ \case
  ModifyActions n -> effectPipe (modifyActions n) (XModifyActions n)
  ModifyBuys n -> effectPipe (modifyBuys n) (XModifyBuys n)
  ModifyCurrency n -> effectPipe (modifyCurrency n) (XModifyCurrency n)
  ActivateCard pl c -> effectPipe (activateCard pl c) (XActivateCard pl c)
  DrawOnce pl -> effectPipe (drawOnce pl) (XDrawOnce pl)
  BlockOne pl c -> effectPipe (blockOne pl c) (XBlockOne pl c)
  Discard pl c -> effectPipe (discard pl c) (XDiscard pl c)
  TrashCard pl c -> effectPipe (trashCard pl c) (XTrashCard pl c)
  Reveal pl c -> effectPipe (reveal pl c) (XReveal pl c)
  TopDeck pl c -> effectPipe (topDeck pl c) (XTopDeck pl c)
  GainCardTo pl c pos -> effectPipe (gainCardTo pl c pos) (XGainCardTo pl c pos)

logTurn :: Members '[GameLoop, Log Card] r => Sem r a -> Sem r a
logTurn = intercept $ \case
    StartingResources pl -> startingResources pl
    BuyCard pl c -> ifSuccess (buyCard pl c) (logBuy pl c)
    PlayFromHand pl c -> ifSuccess (playFromHand pl c) (logAct pl c)
    PlayTreasure pl c -> ifSuccess (playTreasure pl c) (logTreasure pl c)
    DrawTurnStart pl n -> logPlayerRoundStart pl >> drawTurnStart pl n
    DiscardHandCleanup pl -> discardHandCleanup pl

logToPlayerLog :: (Members '[LogToPlayer (Either Card ObscuredCard), BoardStateRead, Obscure] r) => Sem (Log Card : r) a -> Sem r a
logToPlayerLog = interpret $ \case
  LogPlayerRoundStart player -> logAll (LogPlayerRoundStart player)
  LogBuy player cf -> logAll (LogBuy player cf)
  LogAct player cf -> logAll (LogAct player cf)
  LogTreasure player cf -> logAll (LogTreasure player cf)
  LogEffect (XModifyActions n (Ans m)) -> void $ logEffectAll (XModifyActions n (Ans m))
  LogEffect (XModifyBuys n (Ans m)) -> void $ logEffectAll (XModifyBuys n (Ans m))
  LogEffect (XModifyCurrency n (Ans m)) -> void $ logEffectAll (XModifyCurrency n (Ans m))
  LogEffect (XActivateCard pl c (Ans ())) -> void $ logEffectAll (XActivateCard pl c (Ans ()))
  LogEffect (XDrawOnce pl (Ans mc)) -> logRedactedEff pl (XDrawOnce pl . Ans <$> traverse dontRedact mc) (XDrawOnce pl . Ans <$> traverse redactEff mc)
  LogEffect (XBlockOne pl c (Ans ())) -> void $ logEffectAll (XBlockOne pl c (Ans ()))
  LogEffect (XDiscard pl c (Ans ())) -> logRedactedEff pl ((\x -> XDiscard pl x (Ans ())) <$> dontRedact c) ((\x -> XDiscard pl x (Ans ())) <$> redactEff c)
  LogEffect (XTrashCard pl c (Ans ())) -> logRedactedEff pl ((\x -> XTrashCard pl x (Ans ())) <$> dontRedact c) ((\x -> XTrashCard pl x (Ans ())) <$> redactEff c)
  LogEffect (XReveal pl c (Ans ())) -> void $ logEffectAll (XReveal pl c (Ans ()))
  LogEffect (XTopDeck pl c (Ans ())) -> logRedactedEff pl ((\x -> XTopDeck pl x (Ans ())) <$> dontRedact c) ((\x -> XTopDeck pl x (Ans ())) <$> redactEff c)
  LogEffect (XGainCardTo pl cf pos (Ans mcard)) -> logRedactedEff pl (XGainCardTo pl cf pos . Ans <$> traverse dontRedact mcard) (XGainCardTo pl cf pos . Ans <$> traverse redactEff mcard)
  where
    logAll :: (Members '[LogToPlayer (Either Card ObscuredCard), BoardStateRead] r) => Log Card (Sem r) () -> Sem r ()
    logAll x = void $ applyToAll (logToPlayer (logCardMap Left x))

    logEffectAll :: (Members '[LogToPlayer (Either Card ObscuredCard), BoardStateRead] r) => CardEffects'' Card -> Sem r ()
    logEffectAll a = void $ applyToAll $ logToPlayer . logCardMap Left $ LogEffect a

    dontRedact :: Card -> Sem r (Either Card ObscuredCard)
    dontRedact = return . Left

    redactEff :: Member Obscure r => Card -> Sem r (Either Card ObscuredCard)
    redactEff = fmap Right . obscure

    logRedactedEff :: (Members '[LogToPlayer (Either Card ObscuredCard), BoardStateRead] r) =>
                   Player -> Sem r (CardEffects'' (Either Card ObscuredCard)) -> Sem r (CardEffects'' (Either Card ObscuredCard)) -> Sem r ()
    logRedactedEff pl secret public = do
      secret <- secret
      public <- public
      _ <- logToPlayer (LogEffect secret) pl
      _ <- applyToOthers pl (logToPlayer (LogEffect public))
      return ()

logPlayerToString :: (Member (Output (Player, String)) r, Show a) => Sem (LogToPlayer card: r) a -> Sem r a
logPlayerToString = interpret $ \case
  LogToPlayer (LogPlayerRoundStart pl) logpl -> output (logpl, show (LogPlayerRoundStart @Card pl))
  LogToPlayer (LogBuy pl cf) logpl -> output (logpl, show (LogBuy @Card pl cf))
  LogToPlayer (LogAct pl c) logpl -> output (logpl, show (LogAct pl c))
  LogToPlayer (LogTreasure pl c) logpl -> output (logpl, show (LogTreasure pl c))
  LogToPlayer (LogEffect eff) logpl -> output (logpl, show (LogEffect eff))
