module Data where

import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Effects
import Types
import Data.List

initialBaseSupply :: Int -> Map CardFace Int
initialBaseSupply 2 = Map.fromList [
  (Copper,   60),
  (Silver,   40),
  (Gold,     30),
  (Estate,   8) ,
  (Duchy,    8) ,
  (Province, 8) ,
  (Curse,    10)]
initialBaseSupply 3 = Map.fromList [
  (Copper,   60),
  (Silver,   40),
  (Gold,     30),
  (Estate,   12) ,
  (Duchy,    12) ,
  (Province, 12) ,
  (Curse,    20)]
initialBaseSupply 4 = Map.fromList [
  (Copper,   60),
  (Silver,   40),
  (Gold,     30),
  (Estate,   12) ,
  (Duchy,    12) ,
  (Province, 12) ,
  (Curse,    30)]
initialBaseSupply 5 = Map.fromList [
  (Copper,   120),
  (Silver,   80),
  (Gold,     60),
  (Estate,   12) ,
  (Duchy,    12) ,
  (Province, 15) ,
  (Curse,    40)]
initialBaseSupply 6 = Map.fromList [
  (Copper,   120),
  (Silver,   80),
  (Gold,     60),
  (Estate,   12) ,
  (Duchy,    12) ,
  (Province, 18) ,
  (Curse,    50)]
initialBaseSupply _ = undefined

initialMap :: [Player] -> [CardFace] -> Map CardFace Int
initialMap players kingdomCards = initialBaseSupply (length players) `Map.union` constMap kingdomCards 10

boardInitState :: [Player] -> [CardFace] -> Map Position [(CardFace, Int)]
boardInitState pl cf = Map.unions [setPlayerCards, initPlayerPos, initSetSupply, initSetTrash]
  where
    initPlayerPos = Map.fromList $ map (\x -> (x,[])) $ liftA2 PlayerCard pl allPositions
    setPlayerCards = Map.fromList [(PlayerCard p PlayerDeck, [(Estate, 3)]) | p <- pl]
    initSetSupply = Map.mapKeys Supply $ fmap singleton $ Map.mapWithKey (,) $ initialMap pl cf
    initSetTrash = Map.fromList [(Trash, [])]