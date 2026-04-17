module GameLoop where

import Polysemy
import Control.Monad.Loops ( anyM, unfoldM )
import Control.Monad
import Data.Function
--import Data.Map (Map)
import qualified Data.Map as Map

import Types
import Effects
-- discardHand' :: (Member CardEffects r, Member BoardStateRead r) => Player -> Sem r ()
-- discardHand' player = void $ applyTo (discard player) (getHand player)

-- Prompt the player to act, Maybe signals choosing to not act
playOneAction' :: (Member GameLoop r, Member PlayerIO r) => Player -> Sem r (Maybe Card) -> Sem r (Maybe Card)
playOneAction' player if_invalid = do
  mcard <- getAction player
  case mcard of
    Nothing -> return Nothing
    Just card -> do
      mplay <- playFromHand player card
      case mplay of
        Left _ -> if_invalid
        Right () -> return $ Just card

-- Prompt the player to buy, Maybe signals choosing to not buy
playOneBuy' :: (Member GameLoop r, Member PlayerIO r) => Player -> Sem r (Maybe Card) -> Sem r (Maybe Card)
playOneBuy' player if_invalid = do
  mcardface <- getBuy player
  case mcardface of
    Nothing -> return Nothing
    Just cardface -> do
     mcard <- buyCard player cardface
     case mcard of
      Left _   -> if_invalid
      Right card -> return $ Just card

playOneTreasure' :: (Member GameLoop r, Member PlayerIO r) => Player -> Sem r (Maybe Int) -> Sem r (Maybe Int)
playOneTreasure' player if_invalid = do
    mtreasure <- getPlayTreasure player
    case mtreasure of
        Nothing -> return Nothing
        Just card -> do
            msuccess <- playTreasure player card
            case msuccess of
                Left _ -> if_invalid
                Right n -> return $ Just n

playOneAction :: (Member GameLoop r, Member PlayerIO r) => Player -> Sem r (Maybe Card)
playOneAction player = fix $ playOneAction' player

playOneTreasure :: (Member GameLoop r, Member PlayerIO r) => Player -> Sem r (Maybe Int)
playOneTreasure player = fix $ playOneTreasure' player

playOneBuy :: (Member GameLoop r, Member PlayerIO r) => Player -> Sem r (Maybe Card)
playOneBuy player = fix $ playOneBuy' player

repeatAction :: Monad m => m (Maybe a) -> m [a]
repeatAction = unfoldM

newHand :: Member GameLoop r => Player -> Sem r [Card]
newHand player = discardHandCleanup player >> drawTurnStart player 5

-- Bool signals game over
playerRound :: Members '[GameLoop, BoardStateRead, PlayerIO] r => Player -> Sem r Bool
playerRound player =
  startingResources player >>
  repeatAction (playOneAction player) >>
  repeatAction (playOneTreasure player) >>
  repeatAction (playOneBuy player) >>
  newHand player >>
  isGameOver

setInitialGameState :: Members '[GameLoop, CardEffects, BoardStateRead] r => Sem r ()
setInitialGameState = do
  players <- Map.keys <$> getPlayers
  replicateM_ 5 $ forM players (`gainCard` Copper)
  forM_ players newHand

playUntilGameOver :: Monad m => (player -> m Bool) -> [player] -> m ()
playUntilGameOver f xs = void $ anyM f xs

playGame :: Members '[GameLoop, BoardStateRead, PlayerIO, CardEffects] r => Sem r ()
playGame = do
    players <- Map.keys <$> getPlayers
    setInitialGameState
    playUntilGameOver playerRound (cycle players)