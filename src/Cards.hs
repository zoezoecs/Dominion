module Cards where

import Polysemy
import Control.Monad
import Data.Maybe ( catMaybes )
import Data.List ( (\\) )
import qualified Data.Map as Map

import Effects
import Base
import Types

-- TODO: Think about extensibility vs guarantees with where this data goes...
getFace :: Card -> CardFace
getFace (MkCard _ face) = face

getCardVP :: Card -> Int
getCardVP = getFaceVP . getFace
getFaceVP :: CardFace -> Int
getFaceVP Province = 6
getFaceVP Duchy = 3
getFaceVP Estate = 1
getFaceVP _ = 0

-- Nothing represents not having a Treasure value
-- 0 represents being a treasure that provides 0 value
getCurrency :: Card -> Maybe Int
getCurrency = getFaceCurrency . getFace
getFaceCurrency :: CardFace -> Maybe Int
getFaceCurrency Gold = Just 3
getFaceCurrency Silver = Just 2
getFaceCurrency Copper = Just 1
getFaceCurrency _ = Nothing

getCost :: Card -> Int
getCost = getFaceCost . getFace

getFaceCost :: CardFace -> Int
getFaceCost = undefined

getTypes :: CardFace -> [CardTypes]
getTypes = undefined
getReaction :: CardFace -> Reaction m ()
getReaction = undefined
getEffect :: CardFace -> CardSemantics
getEffect = undefined

--bandit :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r) => Player -> Sem r ()
bandit :: CardSemantics
bandit player _ = do
  _ <- gainCard player Gold
  players <- getPlayers
  forM_ (dupKey $ Map.delete player players) bandited

bandited :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r) => Player -> Sem r ()
bandited player = do
  mcard0 <- getTopNCard player 0
  mcard1 <- getTopNCard player 1
  let cards = catMaybes [mcard0, mcard1]
  forM_ cards (reveal player)
  let nonCopperTreasure = filter ((/= Copper) . getFace) cards
  toTrash <- getTrashExactlyN player 1 nonCopperTreasure
  forM_ toTrash (trashCard player)
  forM_ (cards \\ toTrash) (discard player)

--witch :: (Member CardEffects r) => Player -> Sem r ()
witch :: CardSemantics
witch player _ = do
  _ <- drawCard player 1
  _ <- modifyActions 1
  _ <- applyToOthers player (`gainCard` Curse)
  return ()

-- moatPlay :: (Member CardEffects r) => Player -> Sem r ()
moatPlay :: CardSemantics
moatPlay player _ = void $ drawCard player 2

isAttack :: CardFace -> Bool
isAttack face = CardAttack `elem` getTypes face

otherPlayerAttack :: Player -> CardEffects r a -> Bool
otherPlayerAttack player (ActivateCard pl card) = (player /= pl) && isAttack (getFace card)
otherPlayerAttack _ _ = False

-- Uhhhh TODO is this correct? Reaction as an effect?
moatReact :: (Members '[CardEffects, Reaction] r) => Player -> Card -> Sem r ()
moatReact player card = reaction (otherPlayerAttack player) moatBlock
  where
    moatBlock = do
      reveal player card
      blockOne player card

--councilRoom :: (Member CardEffects r) => Player -> Sem r ()
councilRoom :: CardSemantics
councilRoom player _ = do
  _ <- drawCard player 4
  _ <- modifyBuys 1
  _ <- applyToOthers player (`drawCard` 1)
  return ()