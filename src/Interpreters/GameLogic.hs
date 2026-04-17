module Interpreters.GameLogic where

import Polysemy
import Polysemy.State

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Types
import Effects
import Cards
import Debug.Trace

interpGameLoop :: Members '[Stacks, State GameState, BoardStateRead, GameRules, CardEffects] r => Sem (GameLoop : r) a -> Sem r a
interpGameLoop = interpret $ \case
  StartingResources _ -> do -- Starting player is implicit in game state
    modify (\gs -> gs
      { current_actions  = 1
      , current_buys     = 1
      , current_currency = 0
      })
  BuyCard player face -> do
    valid_buy <- canBuy player face
    case valid_buy of
      Left err -> return $ Left err
      Right () -> do
        mcard <- gainCard player face
        case mcard of
          Left err -> return $ Left (BadGain err)
          Right card -> do modify (modBuys (-1))
                           return $ Right card
  PlayFromHand player card -> do
    valid_action <- canAct player card
    case valid_action of
      Left err -> return $ Left err
      Right () -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify (modActions (-1))
        activateCard player card
        return $ Right ()
  PlayTreasure player card -> do
    valid_treasure <- canTreasure player card
    case valid_treasure of
      Right n -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify $ modCurrency n
        return $ Right n
      Left err -> trace "weh" $ return $ Left err
  DrawTurnStart pl n -> drawCard pl n
  DiscardHandCleanup pl -> do
    hand <- getHand pl
    forM_ hand (discard pl)
    stackOnto (PlayerCard pl PlayerInPlay) (PlayerCard pl PlayerDiscardPile)
    stackOnto (PlayerCard pl PlayerSetAside) (PlayerCard pl PlayerDiscardPile)

interpDoReaction :: Members '[GameRules] r => Sem (DoReaction : r) a -> Sem r a
interpDoReaction = interpret $ \case
  DoReaction pl card ceff ma -> do
    valid_reaction <- canReact pl card ceff ma
    case valid_reaction of
      Left err -> return . Left $ err
      Right () -> do
        getCardReaction (getFace card)
        return . Right $ ()

interpGameRules :: Members '[State GameState, Stacks, BoardStateRead] r => Sem (GameRules : r) a -> Sem r a
interpGameRules = interpret $ \case
  CanBuy pl face -> do
    gs        <- get @GameState
    stack     <- getStack (Supply face)
    let result
          | current_buys gs <= 0                    = Left NoBuys
          | current_currency gs < getFaceCost face  = Left NoMoney
          | isNothing stack                         = Left $ BadGain NotInKingdom
          | stack == Just []                        = Left $ BadGain EmptySupply
          | otherwise                               = Right ()
    return result
  CanAct pl card -> do
    gs        <- get @GameState
    handCards <- getHand pl
    let result
          | current_actions gs <= 0  = Left NoActions
          | card `notElem` handCards = Left CardPositionIncorrect
          | otherwise                = Right ()
    return result
  CanTreasure pl card -> do
    handCards <- getHand pl
    let result
          | card `notElem` handCards = Left TreasurePositionIncorrect
          | otherwise                = Right ()
    case (result, getCurrency card) of
      (Left err, _) -> return $ Left err
      (Right (), Nothing) -> return $ Left NotATresure
      (Right (), Just n) -> return $ Right n
  CanReact pl card ceff ma -> do
    handCards <- getHand pl
    let cond_true = case (getCardReaction . getFace $ card, ma) of
          (BeforeReaction cond _, Nothing) -> cond (cardEffectrMap ceff)
          (AfterReaction cond _, Just a) -> cond (cardEffectrMap ceff) a
          _ -> False
    let result
          | card `notElem` handCards = Left NoCard
          | cond_true                = Left ConditionNotMet
          | otherwise                = Right ()
    return result
