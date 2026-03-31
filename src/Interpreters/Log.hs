module Interpreters.Log where
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

import Polysemy
import Polysemy.Output

import Control.Monad
import Data.Map (Map)

import Base
import Effects

effectPipe :: (Member (Log Card) r, Member CardEffects r) => Sem r b -> CardEffects r b -> Sem r b
effectPipe a b = a >>=/ (logEffect . showLoggable) b

-- TODO: Fix boilerplate. Its annoying I can't even use @ patterns...
-- TODO: This should be a lot more granular, since not every player gets every event at the same level of knowledge.
logEffects :: Members '[CardEffects, Log Card] r => Sem (CardEffects : r) a -> Sem r a
logEffects = interpret $ \case
  ModifyActions n -> effectPipe (modifyActions n) (ModifyActions n)
  ModifyBuys n -> effectPipe (modifyBuys n) (ModifyBuys n)
  ModifyCurrency n -> effectPipe (modifyCurrency n) (ModifyCurrency n)
  ActivateCard pl c -> effectPipe (activateCard pl c) (ActivateCard pl c)
  DrawOnce pl -> effectPipe (drawOnce pl) (DrawOnce pl)
  BlockOne pl c -> effectPipe (blockOne pl c) (BlockOne pl c)
  Discard pl c -> effectPipe (discard pl c) (Discard pl c)
  TrashCard pl c -> effectPipe (trashCard pl c) (TrashCard pl c)
  Reveal pl c -> effectPipe (reveal pl c) (Reveal pl c)
  TopDeck pl c -> effectPipe (topDeck pl c) (TopDeck pl c)
  GainCardTo pl c pos -> effectPipe (gainCardTo pl c pos) (GainCardTo pl c pos)

logTurn :: Members '[GameLoop, Log Card] r => Sem r a -> Sem r a
logTurn = intercept $ \case
    StartingResources pl -> startingResources pl
    BuyCard pl c -> ifSuccess (buyCard pl c) (logBuy pl c)
    PlayFromHand pl c -> ifSuccess (playFromHand pl c) (logAct pl c)
    PlayTreasure pl c -> ifSuccess (playTreasure pl c) (logTreasure pl c)
    DrawTurnStart pl n -> logPlayerRoundStart pl >> drawTurnStart pl n
    DiscardHandCleanup pl -> discardHandCleanup pl

logToPlayerLog :: (Show a, Members '[LogToPlayer, BoardStateRead] r) => Sem (Log Card : r) a -> Sem r a
logToPlayerLog = interpret $ \case
  LogPlayerRoundStart player -> logAll (LogPlayerRoundStart player)
  LogBuy player cf -> logAll (LogBuy player cf)
  LogAct player cf -> logAll (LogAct player cf)
  LogTreasure player cf -> logAll (LogTreasure player cf)
  LogEffect (LogEvent (ModifyActions n)) m -> logEffectAll (ModifyActions n) m >> return m
  LogEffect (LogEvent (ModifyBuys n)) m -> logEffectAll (ModifyBuys n) m >> return m
  LogEffect (LogEvent (ModifyCurrency n)) m -> logEffectAll (ModifyCurrency n) m >> return m
  LogEffect (LogEvent (ActivateCard pl c)) () -> void $ logEffectAll (ActivateCard pl c) ()
  LogEffect (LogEvent (DrawOnce pl)) mc -> logRedacted pl (DrawOnce pl) mc (DrawOnce pl) (redact <$> mc) >> return mc
  LogEffect (LogEvent (BlockOne pl c)) () -> void $ logEffectAll (BlockOne pl c) ()
  LogEffect (LogEvent (Discard pl c)) () -> logRedacted pl (Discard pl c) () (Discard pl (redact c)) ()
  LogEffect (LogEvent (TrashCard pl c)) () -> logRedacted pl (TrashCard pl c) () (TrashCard pl (redact c)) ()
  LogEffect (LogEvent (Reveal pl c)) () -> void $ logEffectAll (Reveal pl c) ()
  LogEffect (LogEvent (TopDeck pl c)) () -> logRedacted pl (TopDeck pl c) () (TopDeck pl (redact c)) ()
  LogEffect (LogEvent (GainCardTo pl cf pos)) mcard -> logRedacted pl (GainCardTo pl cf pos) mcard (GainCardTo pl cf pos) (redact <$> mcard) >> return mcard
  where
    logAll :: (Members '[LogToPlayer, BoardStateRead] r) => Log card (Sem r) () -> Sem r ()
    logAll x = void $ applyToAll (logToPlayer x)

    logEffectAll :: (Show a, Members '[LogToPlayer, BoardStateRead] r) => CardEffects' Card m a -> a -> Sem r (Map Player a)
    logEffectAll a b = applyToAll $ logToPlayer $ LogEffect (LogEvent a) b

    redact :: Card -> ObscuredCard
    redact = undefined

    logRedacted :: (Show card, Show a, Show b, Members '[LogToPlayer, BoardStateRead] r) => 
                   Player -> CardEffects' Card m a -> a -> CardEffects' card m b -> b -> Sem r ()
    logRedacted pl secret secret_val public public_val = do
      _ <- logToPlayer (LogEffect (LogEvent secret) secret_val) pl
      _ <- applyToOthers pl (logToPlayer (LogEffect (LogEvent public) public_val))
      return ()

logPlayerToString :: (Member (Output (Player, String)) r, Show a) => Sem (LogToPlayer : r) a -> Sem r a
logPlayerToString = interpret $ \case
  LogToPlayer (LogPlayerRoundStart pl) logpl -> output (logpl, show (LogPlayerRoundStart @Card pl))
  LogToPlayer (LogBuy pl cf) logpl -> output (logpl, show (LogBuy @Card pl cf))
  LogToPlayer (LogAct pl c) logpl -> output (logpl, show (LogAct pl c))
  LogToPlayer (LogTreasure pl c) logpl -> output (logpl, show (LogTreasure pl c))
  LogToPlayer (LogEffect (LogEvent eff) x) logpl -> output (logpl, show (LogEffect (LogEvent eff) x, x)) >> return x
