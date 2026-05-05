module Cards where

import Polysemy
import Control.Monad
import Control.Monad.Loops
import Data.Maybe
import Data.Functor.Const
import Data.List

import Effects
import Base
import Types
import TypesSecret

type FaceInfo = FaceInfo' VictoryPoints CardTypes CardReactionSemantics CardSemantics

getFaceInfo :: CardFace -> FaceInfo
getFaceInfo Copper      = FaceInfo (plainVP 0) (Just 1) 0 [CardTreasure] Nothing Nothing
getFaceInfo Silver      = FaceInfo (plainVP 0) (Just 2) 3 [CardTreasure] Nothing Nothing
getFaceInfo Gold        = FaceInfo (plainVP 0) (Just 3) 6 [CardTreasure] Nothing Nothing
getFaceInfo Estate      = FaceInfo (plainVP 1) Nothing 2 [CardVictory] Nothing Nothing
getFaceInfo Duchy       = FaceInfo (plainVP 3) Nothing 5 [CardVictory] Nothing Nothing
getFaceInfo Province    = FaceInfo (plainVP 6) Nothing 8 [CardVictory] Nothing Nothing
getFaceInfo Curse       = FaceInfo (plainVP (-1)) Nothing 0 [CardCurse] Nothing Nothing
getFaceInfo Cellar      = FaceInfo (plainVP 0) Nothing 2 [CardAction] Nothing (Just $ CardSemantics cellar)
getFaceInfo Chapel      = FaceInfo (plainVP 0) Nothing 2 [CardAction] Nothing (Just $ CardSemantics chapel)
getFaceInfo Moat        = FaceInfo (plainVP 0) Nothing 2 [CardAction, CardReaction] (Just (CardReactionSemantics moatReact)) (Just $ CardSemantics moat)
getFaceInfo Harbinger   = FaceInfo (plainVP 0) Nothing 3 [CardAction] Nothing (Just $ CardSemantics harbinger)
getFaceInfo Merchant    = FaceInfo (plainVP 0) Nothing 3 [CardAction] Nothing (Just $ CardSemantics merchant)
getFaceInfo Vassal      = FaceInfo (plainVP 0) Nothing 3 [CardAction] Nothing (Just $ CardSemantics vassal)
getFaceInfo Village     = FaceInfo (plainVP 0) Nothing 3 [CardAction] Nothing (Just $ CardSemantics village)
getFaceInfo Workshop    = FaceInfo (plainVP 0) Nothing 3 [CardAction] Nothing (Just $ CardSemantics workshop)
getFaceInfo Bureaucrat  = FaceInfo (plainVP 0) Nothing 4 [CardAction, CardAttack] Nothing (Just $ CardSemantics bureaucrat)
getFaceInfo Gardens     = FaceInfo (VPS [GardensVP]) Nothing 4 [CardVictory] Nothing Nothing
getFaceInfo Militia     = FaceInfo (plainVP 0) Nothing 4 [CardAction, CardAttack] Nothing (Just $ CardSemantics militia)
getFaceInfo Moneylender = FaceInfo (plainVP 0) Nothing 4 [CardAction] Nothing (Just $ CardSemantics moneylender)
getFaceInfo Poacher     = FaceInfo (plainVP 0) Nothing 4 [CardAction] Nothing (Just $ CardSemantics poacher)
getFaceInfo Remodel     = FaceInfo (plainVP 0) Nothing 4 [CardAction] Nothing (Just $ CardSemantics remodel)
getFaceInfo Smithy      = FaceInfo (plainVP 0) Nothing 4 [CardAction] Nothing (Just $ CardSemantics smithy)
getFaceInfo ThroneRoom  = FaceInfo (plainVP 0) Nothing 4 [CardAction] Nothing (Just $ CardSemantics throneRoom)
getFaceInfo Bandit      = FaceInfo (plainVP 0) Nothing 5 [CardAction, CardAttack] Nothing (Just $ CardSemantics bandit)
getFaceInfo CouncilRoom = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics councilRoom)
getFaceInfo Festival    = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics festival)
getFaceInfo Laboratory  = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics laboratory)
getFaceInfo Library     = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics library)
getFaceInfo Market      = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics market)
getFaceInfo Mine        = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics mine)
getFaceInfo Sentry      = FaceInfo (plainVP 0) Nothing 5 [CardAction] Nothing (Just $ CardSemantics sentry)
getFaceInfo Witch       = FaceInfo (plainVP 0) Nothing 5 [CardAction, CardAttack] Nothing (Just $ CardSemantics witch)
getFaceInfo Artisan     = FaceInfo (plainVP 0) Nothing 6 [CardAction] Nothing (Just $ CardSemantics artisan)

getFace :: Card -> CardFace
getFace (MkCard _ face) = face

getCardVP :: Card -> VictoryPoints
getCardVP = getFaceVP . getFace
getFaceVP :: CardFace -> VictoryPoints
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

getTypes :: Card -> [CardTypes]
getTypes = getFaceTypes . getFace

getFaceTypes :: CardFace -> [CardTypes]
getFaceTypes = getFaceTypes' . getFaceInfo

unknownLookupReaction :: CardFace -> Maybe HasReaction
unknownLookupReaction = unknownLookupReaction' . getFaceInfo

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
  Nothing -> \_ _ -> pure ()
  Just x -> getSemantics x


hasFaceType :: CardTypes -> CardFace -> Bool
hasFaceType ct card = ct `elem` getFaceTypes card

hasType :: CardTypes -> Card -> Bool
hasType ct card = ct `elem` getTypes card

isFace :: CardFace -> Card -> Bool
isFace fc = (fc ==) . getFace

isAttack :: Card -> Bool
isAttack = hasType CardAttack

isVictory :: Card -> Bool
isVictory = hasType CardVictory

isAction :: Card -> Bool
isAction = hasType CardAction

isTreasure :: Card -> Bool
isTreasure = hasType CardTreasure

isTreasureF :: CardFace -> Bool
isTreasureF = hasFaceType CardTreasure

otherPlayerAttack :: Player -> CardEffects r a -> Bool
otherPlayerAttack player (ActivateCard pl card) = (player /= pl) && isAttack card
otherPlayerAttack _ _ = False



gainToSt :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r, Member Stacks r) => PlayerPosition -> Player -> (CardFace -> Bool) -> Sem r (Either InvalidGain Card)
gainToSt ppos player cond = do
  supplies <- activeSupplies
  cf <- getCardFaceTEMP player (filter cond supplies)
  gainCardTo player cf ppos

gainSt :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r, Member Stacks r) => Player -> (CardFace -> Bool) -> Sem r (Either InvalidGain Card)
gainSt = gainToSt PlayerDiscardPile



witch :: CardSemantics'
witch player _ = do
  _ <- drawOnce player
  _ <- modifyActions 1
  _ <- applyToOthers player (`gainCard` Curse)
  pure ()

moat :: CardSemantics'
moat player _ = void $ drawCard player 2

moatReact :: (Members '[CardEffects] r) => Player -> Card -> Reaction (Sem r) ()
moatReact player card = BeforeReaction (otherPlayerAttack player) moatBlock
  where
    moatBlock = do
      reveal player card
      blockOne player card

councilRoom :: CardSemantics'
councilRoom player _ = do
  _ <- drawCard player 4
  _ <- modifyBuys 1
  _ <- applyToOthers player drawOnce
  pure ()

smithy :: CardSemantics'
smithy player _ = void $ drawCard player 3

village :: CardSemantics'
village player _ = void $ drawCard player 1 >> modifyActions 2

throneRoom :: CardSemantics'
throneRoom player card = void $ do
  hand <- getHand player
  mcard <- getMCardTEMP player (delete card hand)
  forM_ mcard (replicateM 2 . activateCard player)

laboratory :: CardSemantics'
laboratory player _ = void $ do
  _ <- modifyActions 1
  drawCard player 2

festival :: CardSemantics'
festival _ _ = void $ do
  _ <- modifyActions 2
  _ <- modifyBuys 1
  modifyCurrency 2

market :: CardSemantics'
market player _ = void $ do
  _ <- modifyActions 1
  _ <- modifyBuys 1
  _ <- modifyCurrency 1
  drawCard player 1


workshop :: CardSemantics'
workshop player _ = void $ do
  let canGain face = getFaceCost face <= 4
  outcome <- gainSt player canGain
  pure ()

-- TODO: Make ergonomic to express:
-- select (exactly n, up to n, one, maybe one) from hand to (discard/trash/play/topdeck)
-- you may play it
-- check top cards on a pile

-- Just optimise picking cards from hands
cellar :: CardSemantics'
cellar player _ = void $ do
  _ <- modifyActions 1
  hand <- getHand player
  cards <- getCardsTEMP player hand
  forM_ cards (discard player)
  drawCard player (length cards)

chapel :: CardSemantics'
chapel player _ = void $ do
  _ <- modifyActions 1
  hand <- getHand player
  cards <- getUpToNCardsTEMP player 4 hand
  forM_ cards (trashCard player)

moneylender :: CardSemantics'
moneylender player _ = do
  hand <- getHand player
  mcopper <- getMCardTEMP player (filter (isFace Copper) hand)
  forM_ mcopper (\c -> trashCard player c >> modifyCurrency 3)

mine :: CardSemantics'
mine player _ = do
  hand <- getHand player
  mtrash <- getMCardTEMP player (filter isTreasure hand)
  let canGain c face = getFaceCost face <= (getCost c + 3) && isTreasureF face
  forM_ mtrash (\c -> do
    trashCard player c
    gainSt player (canGain c))

remodel :: CardSemantics'
remodel player _ = void $ do
  hand <- getHand player
  toTrash <- getCardTEMP player hand
  trashCard player toTrash
  let canGain face = getFaceCost face <= (getCost toTrash + 2)
  outcome <- gainSt player canGain
  pure ()

bureaucrat :: CardSemantics'
bureaucrat player _ = void $ do
  _ <- gainCardTo player Silver PlayerDeck
  applyToOthers player bureaucrated

bureaucrated :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r, Member Stacks r) => Player -> Sem r ()
bureaucrated player = do
  hand <- getHand player
  let victories = filter isVictory hand
  case null victories of
    True -> forM_ hand (reveal player)
    False -> do
      toShow <- getCardTEMP player victories
      reveal player toShow
      topDeck player toShow

-- Get from hand to do something else
artisan :: CardSemantics'
artisan player _ = void $ do
  let canGain face = getFaceCost face <= 5
  outcome <- gainToSt PlayerHand player canGain
  hand <- getHand player
  toTopdeck <- getCardTEMP player hand
  topDeck player toTopdeck

-- Harder:
poacher :: CardSemantics'
poacher player _ = void $ do
  _ <- modifyActions 1
  _ <- modifyCurrency 1
  _ <- drawCard player 1
  hand <- getHand player
  empty_supplies <- numEmptySupplies
  to_discard <- getNCardsTEMP player empty_supplies hand
  forM_ to_discard (discard player)

harbinger :: CardSemantics'
harbinger player _ = void $ do
  discards <- getDiscardPile player
  sendStack PlayerDiscardPile discards
  mcard <- getMCardTEMP player discards
  forM_ mcard (topDeck player)

militia :: CardSemantics'
militia player _ = void $ do
  _ <- modifyCurrency 2
  hand <- getHand player
  keep_cards <- getNCardsTEMP player 3 hand
  forM_ (hand \\ keep_cards) (discard player)

vassal :: CardSemantics'
vassal player _ = void $ do
  _ <- modifyCurrency 2
  mcard <- getTopCard player
  case mcard of
    Nothing -> pure ()
    Just card -> discard player card >> when (CardAction `elem` getTypes card) (void $ do
      mcard2 <- getMCardTEMP player [card]
      traverse (activateCard player) mcard2)

library :: CardSemantics'
library player _ = void $ do
  skipped_cards <- whileM (liftA2 (&&) (canDraw player) ((7 >=) . length <$> getHand player)) (libraryDraw player)
  forM (catMaybes skipped_cards) (discard player)

libraryDraw :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r, Member Stacks r) => Player -> Sem r (Maybe Card)
libraryDraw player = do
  mcard  <- mTop <$> getStack (PlayerCard player PlayerDeck) -- TODO: also wrong, won't reshuffle if the deck empties
  case mcard of
    Nothing -> pure Nothing
    Just card -> do
      toSkip <- getMCardTEMP player (filter isAction [card])
      case toSkip of
        Nothing -> drawOnce player
        Just skip -> putPlay player skip >> pure (Just skip)

sentry :: CardSemantics'
sentry player _ = do
  _ <- drawCard player 1
  _ <- modifyActions 1
  deck <- getStack (PlayerCard player PlayerDeck)
  let topTwo = take 2 . concat $ deck -- TODO: Incorrect. Should draw from discard if needed, and the code for this should be somewhere else.  Ensure n?
  toTrash <- getCardsTEMP player topTwo
  toDiscard <- getCardsTEMP player (topTwo \\ toTrash)
  anyOrder <- getCardsTEMP player ((topTwo \\ toTrash) \\ toDiscard)
  forM_ toTrash (trashCard player)
  forM_ toDiscard (discard player)
  forM_ anyOrder (topDeck player)

merchant :: CardSemantics'
merchant player _ = do
  _ <- drawOnce player
  _ <- modifyActions 1
  undefined

bandit :: CardSemantics'
bandit player _ = void $ do
  _ <- gainCard player Gold
  applyToOthers player bandited

bandited :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r) => Player -> Sem r ()
bandited player = do
  mcard0 <- getTopNCard player 0
  mcard1 <- getTopNCard player 1
  let cards = catMaybes [mcard0, mcard1]
  forM_ cards (reveal player)
  let nonCopperTreasure = filter ((/= Copper) . getFace) cards
  toTrash <- if null nonCopperTreasure then pure [] else singleton <$> getCardTEMP player nonCopperTreasure
  forM_ toTrash (trashCard player)
  forM_ (cards \\ toTrash) (discard player)
