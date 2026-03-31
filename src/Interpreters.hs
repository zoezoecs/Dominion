module Interpreters where

import Polysemy
import Polysemy.State
import Polysemy.Input
import Polysemy.Output

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

import System.Random.Shuffle
import System.Random
import System.Random.Stateful

import Data.Functor.Identity
import GHC.Conc
import qualified Control.Monad.State as CMS

import Base
import Effects
import Cards

-- Obvious design choice: state is a big datatype
data GameState = MkGameState {
  all_players :: [Player],
  blocks :: Map Player Bool,
  current_actions :: Int,
  current_buys :: Int,
  current_currency :: Int
  -- reactions :: [Reaction m]
}
modActions n gs = gs{current_actions=n+current_actions gs}
modBuys n gs = gs{current_buys=n+current_buys gs}
modCurrency n gs = gs{current_currency=n+current_currency gs}
setBlocks :: Player -> Bool -> GameState -> GameState
setBlocks pl b gs = gs{blocks=Map.insert pl b (blocks gs)}


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

interpCardEffects :: Members '[Stacks, State GameState, PlayerIO, BoardStateRead] r => Sem (CardEffects : r) a -> Sem r a
interpCardEffects = interpret $ \case
  ModifyActions n -> modify (modActions n) >> current_actions <$> get
  ModifyBuys n -> modify (modBuys n) >> current_buys <$> get
  ModifyCurrency n -> modify (modCurrency n) >> current_currency <$> get
  ActivateCard pl c -> interpCardEffects (getEffect (getFace c) pl c) -- Moat check and reaction checks. Isn't it weird c appears twice?
  DrawOnce pl -> drawTo (PlayerCard pl PlayerDeck) (PlayerCard pl PlayerHand)
  BlockOne pl _ -> void $ modify (setBlocks pl True)
  Discard pl c -> void $ cardToPos c (PlayerCard pl PlayerDiscardPile)
  TrashCard _ c -> void $ cardToPos c Trash
  Reveal _ _ -> return () -- Reveal handled elsewhere
  TopDeck pl c -> void $ cardToPos c (PlayerCard pl PlayerDeck)
  GainCardTo pl c pos -> do
    mcard <- drawTo (Supply c) (PlayerCard pl pos)
    case mcard of
      Nothing -> return $ Left EmptySupply
      Just card -> return $ Right card

-- via interceptH as well to check then?
-- cleanup?
interpReaction :: Sem (Reaction : r) a -> Sem r a
interpReaction = interpretH $ \case
  Reaction cond m -> undefined

effectPipe :: (Member Log r, Member CardEffects r) => Sem r b -> CardEffects r b -> Sem r b
effectPipe a b = a >>=/ (logEffect . showLoggable) b

-- TODO: Fix boilerplate. Its annoying I can't even use @ patterns...
-- TODO: This should be a lot more granular, since not every player gets every event at the same level of knowledge.
logEffects :: Members '[CardEffects, Log] r => Sem (CardEffects : r) a -> Sem r a
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

logTurn :: Members '[GameLoop, Log] r => Sem r a -> Sem r a
logTurn = intercept $ \case
    StartingResources pl -> startingResources pl
    BuyCard pl c -> ifSuccess (buyCard pl c) (logBuy pl c)
    PlayFromHand pl c -> ifSuccess (playFromHand pl c) (logAct pl c)
    PlayTreasure pl c -> ifSuccess (playTreasure pl c) (logTreasure pl c)
    DrawTurnStart pl n -> logPlayerRoundStart pl >> drawTurnStart pl n
    DiscardHandCleanup pl -> discardHandCleanup pl

logToString :: (Member (Output String) r, Show a) => Sem (Log : r) a -> Sem r a
logToString = interpret $ \case
  LogPlayerRoundStart pl -> output (show (LogPlayerRoundStart pl))
  LogBuy pl cf -> output (show (LogBuy pl cf))
  LogAct pl c -> output (show (LogAct pl c))
  LogTreasure pl c -> output (show (LogTreasure pl c))
  LogEffect (LogEvent eff) x -> output (show (LogEffect (LogEvent eff) x, x)) >> return x

emptyStack :: Member Stacks r => CardFace -> Sem r Bool
emptyStack face = null <$> getStack (Supply face)

-- TODO: Fix this
justGetStack :: Member Stacks r => Position -> Sem r [Card]
justGetStack p = do
    mstack <- getStack p
    case mstack of
        Nothing -> undefined
        Just cards -> return cards

interpStateRead :: Members '[Stacks, State GameState] r => Sem (BoardStateRead : r) a -> Sem r a
interpStateRead = interpret $ \case
  GetPlayers -> flip constMap () <$> (all_players <$> get)
  GetVP pl -> sum <$> (fmap getCardVP <$> (join <$> mapM (justGetStack . PlayerCard pl) allPositions))
  GetHand pl -> justGetStack (PlayerCard pl PlayerHand)
  GetDeck pl -> justGetStack (PlayerCard pl PlayerDeck)
  GetTopCard pl -> flip (!?) 1 <$> justGetStack (PlayerCard pl PlayerDeck)
  GetTopNCard pl n -> flip (!?) n <$> justGetStack (PlayerCard pl PlayerDeck)
  GetDiscardPile pl -> justGetStack (PlayerCard pl PlayerDiscardPile)
  IsGameOver -> do
    cards <- activeKingdoms
    emptyPiles <- forM cards emptyStack
    provinces <- justGetStack (Supply Province)
    return $ null provinces || countElem True emptyPiles >= 3
  -- GetReactions pl -> _

-- TODO: Fix maybes everywhere
-- TODO: Surely this is a lens type problem. At least modify??
unsafeLookup l = fromJust . Map.lookup l

refill :: (Member Stacks r, Foldable t) => PileConfig t -> Position -> Sem r ()
refill (PileConfig fillFrom shuffleOnFill) l = do
    case Map.lookup l fillFrom of
      Nothing -> return ()
      Just l' -> do
        stackOnto l' l
        when (l `elem` shuffleOnFill) $ shuffleStack l

-- TODO: can i make this use a more efficient list esque representation?
interpStacks :: (Foldable t, Members '[State (Map Position [Card]), RandomShuffle] r) => PileConfig t -> Sem (Stacks : r) a -> Sem r a
interpStacks cfg = interpret $ \case
      ActivePositions -> Map.keys <$> get @(Map Position [Card])
      GetStack loc -> Map.lookup loc <$> get @(Map Position [Card])
      ShuffleStack loc -> do
        cardMap <- get @(Map Position [Card])
        let stack = unsafeLookup loc cardMap
        shuffled <- randomShuffle stack
        put $ Map.insert loc shuffled cardMap
      StackOnto l1 l2 -> do
        cardMap <- get @(Map Position [Card])
        let stack1 = unsafeLookup l1 cardMap
        let stack2 = unsafeLookup l2 cardMap
        put $ Map.insert l1 [] . Map.insert l2 (stack1++stack2) $ cardMap
      DrawTo l1 l2 -> do
        cardMap <- get @(Map Position [Card])
        let stack1 = unsafeLookup l1 cardMap
        let stack2 = unsafeLookup l2 cardMap
        when (null stack1) $ interpStacks cfg $ refill cfg l1
        case stack1 of
            []                -> return Nothing
            (x:xs)            -> do
                put $ Map.insert l1 xs . Map.insert l2 (x:stack2) $ cardMap
                return $ Just x
      CardToPos card loc -> do
        cardMap <- get @(Map Position [Card])
        let notACard = Map.null $ Map.filter (elem card) cardMap
        when notACard undefined
        let newMap = fmap (filter (card ==)) cardMap -- undefined if this doesn't change it?
        let stack1 = unsafeLookup loc newMap
        void $ put $ Map.insert loc (card:stack1) newMap

myShuffle :: StatefulGen g m => g -> [a] -> m [a]
myShuffle gen xs = shuffle xs <$> replicateM (length xs - 1) (uniformRM (0, length xs - 1) gen)

interpRandomShuffle :: Members '[RandomGenEff IO, Embed IO] r => Sem (RandomShuffle : r) a -> Sem r a
interpRandomShuffle = interpret $ \case
  RandomShuffle stack -> do
    HoldRandom gen <- input @(HoldRandom IO)
    embed @IO $ myShuffle gen stack

-- newSTGenM :: g -> ST s (STGenM g s) 
-- MonadIO m => g -> m (AtomicGenM g) 
runInputGenerator :: (Member (Embed m) r) => (g2 -> m (g1 g2)) -> (k -> g2) -> k -> Sem (Input (g1 g2) ': r) a -> Sem r a
runInputGenerator makeGeneral makeBasic n program = do
  gen <- embed $ makeGeneral (makeBasic n)
  runInputConst gen program

runAtomicGenM :: Member (Embed IO) r => Int -> Sem (Input (AtomicGenM StdGen) ': r) a -> Sem r a
runAtomicGenM = runInputGenerator @IO newAtomicGenM mkStdGen

runIOGenM :: Member (Embed IO) r => Int -> Sem (Input (IOGenM StdGen) ': r) a -> Sem r a
runIOGenM = runInputGenerator @IO newIOGenM mkStdGen

runTGenM :: Member (Embed STM) r => Int -> Sem (Input (TGenM StdGen) ': r) a -> Sem r a
runTGenM = runInputGenerator @STM newTGenM mkStdGen

mapInput :: (i1 -> i0) -> Sem (Input i0 : r) a -> Sem (Input i1 : r) a
mapInput f = reinterpret $ \case Input -> f <$> input

constraintIn :: StatefulGen g IO => Sem (Input (HoldRandom IO) : r) a -> Sem (Input g : r) a
constraintIn = mapInput HoldRandom

monomorphiseAtomicGenM :: Sem (Input (HoldRandom IO) : r) a -> Sem (Input (AtomicGenM StdGen) : r) a
monomorphiseAtomicGenM = constraintIn

interpRandomWithSeed :: Members '[Embed IO] r => Int -> Sem (RandomGenEff IO : r) a -> Sem r a
interpRandomWithSeed n = runAtomicGenM n . constraintIn

interpRandomGlobal :: Member (Embed IO) r => Sem (RandomGenEff IO: r) a -> Sem r a
interpRandomGlobal = interpret $ \case
  Input -> return $ HoldRandom globalStdGen


interpCorrelation :: Sem r a -> Sem r a
interpCorrelation = undefined

interpPlayerIO :: Member (Embed IO) r => Sem (PlayerIO : r) a -> Sem r a
interpPlayerIO = undefined