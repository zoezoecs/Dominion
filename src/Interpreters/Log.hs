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
import Interpreters.DoRedact

logEffects :: Members '[CardEffects, Log Card] r => Sem r a -> Sem r a
logEffects = intercept (\cardEff -> send (cardEffectrMap cardEff) >>=/ (logEffect . LogEvent cardEff))

logTurn :: Members '[GameLoop, Log Card] r => Sem r a -> Sem r a
logTurn = intercept $ \case
    StartingResources pl -> startingResources pl
    BuyCard pl c -> ifSuccess (buyCard pl c) (logBuy pl c)
    PlayFromHand pl c -> ifSuccess (playFromHand pl c) (logAct pl c)
    PlayTreasure pl c -> ifSuccess (playTreasure pl c) (logTreasure pl c)
    DrawTurnStart pl n -> logPlayerRoundStart pl >> drawTurnStart pl n
    DiscardHandCleanup pl -> discardHandCleanup pl


redactLogEff :: Member Obscure r => LoggedEvent Card -> Player -> Sem r (LoggedEvent PotentiallyObscured)
redactLogEff ev pl = logEvAnswer <$> redactEvent (evAnswerLog ev) pl

-- We can't write a Traversable instance for Log card m a due to the a being out. Its easier to just manually pipe the cards through here
-- than it would be to make another existential type, which wouldn't even work properly with the effects system
-- So the logging for the non effects is slightly different to logging the LogEffect cardeffect, for which we have a traversable instance.
-- That is why we kind of have two separate log redaction mechanisms, one for cardeffects, and one for the things that aren't cardeffects
logToPlayerLog :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead, Obscure] r) => Sem (Log Card : r) a -> Sem r a
logToPlayerLog = interpret $ \case
  LogPlayerRoundStart player -> logAll0 (LogPlayerRoundStart player)
  LogBuy player cf -> logAll0 (LogBuy player cf)
  LogAct player card -> logAll (LogAct player) card
  LogTreasure player card -> logAll (LogTreasure player) card
  LogEffect a -> void $ do
    players <- getPlayers
    traverse (playerLog a) (dupKey players)
  where
    playerLog :: Members '[Obscure, LogToPlayer PotentiallyObscured] r => LoggedEvent Card -> Player -> Sem r ()
    playerLog ev pl = do
      redacted <- redactLogEff ev pl
      logToPlayer (LogEffect redacted) pl

    dontRedactCard :: Member Obscure r => Card -> Sem r PotentiallyObscured
    dontRedactCard card = fmap (PObscured . Left . \tid -> (card,tid)) . getTempId $ card

    logAll0 :: (Members '[LogToPlayer PotentiallyObscured, BoardStateRead] r) => (forall card. Log card (Sem r) ()) -> Sem r ()
    logAll0 = void . applyToAll . logToPlayer

    logAll :: (Members '[LogToPlayer PotentiallyObscured, Obscure, BoardStateRead] r) => (forall card. card -> Log card (Sem r) ()) -> Card -> Sem r ()
    logAll f x = void $ applyToAll . logToPlayer . f <$> dontRedactCard x

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
