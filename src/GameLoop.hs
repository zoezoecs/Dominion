module GameLoop where

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Control.Monad.Loops ( anyM, unfoldM )
import Control.Monad
import Data.Function
import Data.Either
import Data.Maybe
import Data.List ( (\\) )
import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Effects
import Data
-- discardHand' :: (Member CardEffects r, Member BoardStateRead r) => Player -> Sem r ()
-- discardHand' player = void $ applyTo (discard player) (getHand player)

-- Prompt the player to act, Maybe signals choosing to not act
playOneAction' :: (Member BoardStateEdit r, Member PlayerIO r) => Player -> Sem r (Maybe Card) -> Sem r (Maybe Card)
playOneAction' player if_invalid = do
  mcard <- getAction player
  case mcard of
    Nothing -> return Nothing
    Just card -> do
      mplay <- playFromHand player card
      case mplay of
        Left err -> if_invalid
        Right () -> return $ Just card

-- Prompt the player to buy, Maybe signals choosing to not buy
playOneBuy' :: (Member BoardStateEdit r, Member PlayerIO r) => Player -> Sem r (Maybe Card) -> Sem r (Maybe Card)
playOneBuy' player if_invalid = do
  mcardface <- getBuy player
  case mcardface of
    Nothing -> return Nothing
    Just cardface -> do
     mcard <- buyCard player cardface
     case mcard of
      Left err   -> if_invalid
      Right card -> return $ Just card

playOneAction :: (Member BoardStateEdit r, Member PlayerIO r) => Player -> Sem r (Maybe Card)
playOneAction player = fix $ playOneAction' player

playOneBuy :: (Member BoardStateEdit r, Member PlayerIO r) => Player -> Sem r (Maybe Card)
playOneBuy player = fix $ playOneBuy' player

repeatAction :: Monad m => m (Maybe a) -> m [a]
repeatAction = unfoldM

newHand :: Member BoardStateEdit r => Player -> Sem r [Card]
newHand player = discardHandCleanup player >> drawTurnStart player 5

-- Bool signals game over
playerRound :: (Member BoardStateEdit r, Member BoardStateRead r, Member PlayerIO r, Member PlayerIO r) => Player -> Sem r Bool
playerRound player =
  startingResources player >>
  repeatAction (playOneAction player) >>
  -- TODO: When do I gain dollars?
  repeatAction (playOneBuy player) >>
  newHand player >>
  isGameOver

setInitialGameState :: (Member BoardInit r, Member BoardStateEdit r, Member CardEffects r) => [Player] -> [CardFace] -> Sem r ()
setInitialGameState players kingdomCards = do
  void $ setHand (Map.singleton Estate 3)
  void $ setSupply (initialBaseSupply (length players) `Map.union` constMap kingdomCards 10)
  replicateM_ 5 $ forM players (`gainCard` Copper)
  forM_ players newHand

playUntilGameOver :: Monad m => (player -> m Bool) -> [player] -> m ()
playUntilGameOver f xs = void $ anyM f xs

playGame :: (Member BoardStateEdit r, Member BoardStateRead r, Member BoardInit r, Member PlayerIO r, Member PlayerIO r, Member CardEffects r) => [Player] -> [CardFace] -> Sem r ()
playGame players kingdoms =
  setInitialGameState players kingdoms >>
  playUntilGameOver playerRound (cycle players)