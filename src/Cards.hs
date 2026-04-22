module Cards where

import Polysemy
import Control.Monad
import Data.Maybe
import Data.Functor.Const
import Data.List ( (\\) )
import qualified Data.Map as Map
import Debug.Trace

import Effects
import Base
import Types
import TypesSecret

getFace :: Card -> CardFace
getFace (MkCard _ face) = face

getCardVP :: Card -> Int
getCardVP = getFaceVP . getFace
getFaceVP :: CardFace -> Int
getFaceVP = getFaceVP' . getFaceInfo

-- Nothing represents not having a Treasure value
-- 0 represents being a treasure that provides 0 value
getCurrency :: Card -> Maybe Int
getCurrency = getFaceCurrency . getFace

getFaceCurrency :: CardFace -> Maybe Int
getFaceCurrency = getFaceCurrency' . getFaceInfo

getCost :: Card -> Int
getCost = getFaceCost . getFace

getFaceCost :: CardFace -> Int
getFaceCost = getFaceCost' . getFaceInfo

getTypes :: CardFace -> [CardTypes]
getTypes = getFaceTypes . getFaceInfo

unknownLookupReaction :: CardFace -> Maybe HasReaction
unknownLookupReaction = unknownLookupReaction' . getFaceInfo

type FaceInfo = FaceInfo' CardTypes CardReactionSemantics CardSemantics

knownLookupReaction :: Members '[CardEffects] r => Player -> Card -> HasReaction -> Reaction (Sem r) ()
knownLookupReaction pl c prf = getReactionSemantics (knownLookupReaction' prf (getFaceInfo . getFace $ c)) pl c

knownLookupCond :: Player -> Card -> HasReaction -> Reaction (Const ()) ()
knownLookupCond pl c hr = reactionMap (const (Const ())) blah
  where
    blah :: Reaction (Sem '[CardEffects]) ()
    blah = knownLookupReaction pl c hr

knownLookupCardReactionM :: Members '[CardEffects] r => Player -> Card -> HasReaction -> Sem r ()
knownLookupCardReactionM pl card prf = case knownLookupReaction pl card prf  of
  BeforeReaction _ m -> m
  AfterReaction _ m -> m

getEffect :: CardFace -> CardSemantics'
getEffect cf = case getFaceEffect' . getFaceInfo $ cf of
  Nothing -> \_ _ -> return ()
  Just x -> getSemantics x

getFaceInfo :: CardFace -> FaceInfo
getFaceInfo Bandit = FaceInfo 0 Nothing 5 [CardAction, CardAttack] Nothing (Just $ CardSemantics bandit)
getFaceInfo Moat = FaceInfo 0 Nothing 2 [CardAction, CardReaction] (Just (CardReactionSemantics moatReact)) (Just $ CardSemantics moatPlay)
getFaceInfo Copper = FaceInfo 0 (Just 1) 0 [CardTreasure] Nothing Nothing
getFaceInfo Silver = FaceInfo 0 (Just 2) 3 [CardTreasure] Nothing Nothing
getFaceInfo Gold = FaceInfo 0 (Just 3) 6 [CardTreasure] Nothing Nothing
getFaceInfo Estate = FaceInfo 1 Nothing 2 [CardVictory] Nothing Nothing
getFaceInfo Duchy = FaceInfo 3 Nothing 5 [CardVictory] Nothing Nothing
getFaceInfo Province = FaceInfo 6 Nothing 8 [CardVictory] Nothing Nothing
getFaceInfo Curse = FaceInfo (-1) Nothing 0 [] Nothing Nothing
getFaceInfo card = traceShow card undefined

--bandit :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r) => Player -> Sem r ()
bandit :: CardSemantics'
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
witch :: CardSemantics'
witch player _ = do
  _ <- drawCard player 1
  _ <- modifyActions 1
  _ <- applyToOthers player (`gainCard` Curse)
  return ()

-- moatPlay :: (Member CardEffects r) => Player -> Sem r ()
moatPlay :: CardSemantics'
moatPlay player _ = void $ drawCard player 2

isAttack :: CardFace -> Bool
isAttack face = CardAttack `elem` getTypes face

otherPlayerAttack :: Player -> CardEffects r a -> Bool
otherPlayerAttack player (ActivateCard pl card) = (player /= pl) && isAttack (getFace card)
otherPlayerAttack _ _ = False

moatReact :: (Members '[CardEffects] r) => Player -> Card -> Reaction (Sem r) ()
moatReact player card = BeforeReaction (otherPlayerAttack player) moatBlock
  where
    moatBlock = do
      reveal player card
      blockOne player card

--councilRoom :: (Member CardEffects r) => Player -> Sem r ()
councilRoom :: CardSemantics'
councilRoom player _ = do
  _ <- drawCard player 4
  _ <- modifyBuys 1
  _ <- applyToOthers player (`drawCard` 1)
  return ()