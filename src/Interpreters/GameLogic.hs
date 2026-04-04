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
    case getCurrency card of
      Just n -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify $ modCurrency n
        return $ Right n
      Nothing -> return $ Left NotATresure
  DrawTurnStart pl n -> drawCard pl n
  DiscardHandCleanup pl -> do
    hand <- getHand pl
    forM_ hand (discard pl)
    stackOnto (PlayerCard pl PlayerInPlay) (PlayerCard pl PlayerDiscardPile)
    stackOnto (PlayerCard pl PlayerSetAside) (PlayerCard pl PlayerDiscardPile)

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
