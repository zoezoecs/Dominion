module Interpreters where

import Polysemy
import Polysemy.State
import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Effects
import Cards

-- Obvious design choice: state is a big datatype
data GameState = MkGameState {
  players :: [Player],
  blocks :: Map Player Bool,
  current_player :: Player,
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


interpStateWrite :: Members '[Stacks, State GameState, Log, BoardStateRead, CardEffects] r => Sem (BoardStateEdit : r) a -> Sem r a
interpStateWrite = interpret $ \case
  StartingResources p -> do
    modify (\gs -> gs
      { current_actions  = 1
      , current_buys     = 1
      , current_currency = 0
      })
  BuyCard player face -> do
    gs        <- get @GameState
    stack     <- getStack (Supply face)
    canBuy    <- activeKingdoms
    let maybeError
          | current_buys gs <= 0     = Just NoMoney
          | null stack               = Just $ BadGain EmptySupply
          | face `notElem` canBuy    = Just $ BadGain NotInKingdom
          | otherwise                = Nothing
    case maybeError of
      Just err -> return $ Left err
      Nothing -> do
        mcard <- gainCard player face
        case mcard of
          Left err -> return $ Left (BadGain err) -- TODO: Ugly, fix
          Right card -> do modify (modBuys (-1))
                           return $ Right card
  PlayFromHand player card -> do
    gs        <- get @GameState
    handCards <- getHand player
    let maybeError
          | current_actions gs <= 0  = Just NoActions
          | card `notElem` handCards = Just CardPositionIncorrect
          | otherwise                = Nothing
    case maybeError of
      Just err -> return $ Left err
      Nothing -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify (modActions (-1))
        activateCard player card
        return $ Right ()

  DrawTurnStart pl n -> drawCard pl n
  DiscardHandCleanup pl -> do
    hand <- getHand pl
    forM_ hand (discard pl)
    stackOnto (PlayerCard pl PlayerInPlay) (PlayerCard pl PlayerDiscardPile)
    stackOnto (PlayerCard pl PlayerSetAside) (PlayerCard pl PlayerDiscardPile)

interpCardEffects :: Members '[Stacks, State GameState, Log, PlayerIO, BoardStateRead] r => Sem (CardEffects : r) a -> Sem r a
interpCardEffects = interpret $ \case
  ModifyActions n -> modify (modActions n) >> current_actions <$> get
  ModifyBuys n -> modify (modBuys n) >> current_buys <$> get
  ModifyCurrency n -> modify (modCurrency n) >> current_currency <$> get
  ActivateCard pl c -> interpCardEffects (getEffect (getFace c) pl c) -- Moat check and reaction checks. Isn't it weird c appears twice?
  DrawOnce pl -> do
    let deckloc = PlayerCard pl PlayerDeck
    pdeck <- getStack deckloc
    when (null pdeck) $ stackOnto (PlayerCard pl PlayerDiscardPile) deckloc >> shuffleStack deckloc
    drawTo deckloc (PlayerCard pl PlayerHand)
  BlockOne pl -> void $ modify (setBlocks pl True)
  Discard pl c -> void $ cardToPos c (PlayerCard pl PlayerDiscardPile)
  TrashCard pl c -> void $ cardToPos c Trash
  Reveal pl c -> void $ logReveal pl c
  TopDeck pl c -> void $ cardToPos c (PlayerCard pl PlayerDeck)
  GainCardTo pl c pos -> do
    mcard <- drawTo (Supply c) (PlayerCard pl pos)
    case mcard of
      Nothing -> undefined
      Just card -> return $ Right card

emptyStack :: Member Stacks r => CardFace -> Sem r Bool
emptyStack face = null <$> getStack (Supply face)

-- TODO: Fix this
justGetStack :: Member Stacks r => Position -> Sem r [Card]
justGetStack p = do
    mstack <- getStack p
    case mstack of
        Nothing -> undefined
        Just cards -> return cards

interpStateRead :: Members '[Stacks, State GameState, Log] r => Sem (BoardStateRead : r) a -> Sem r a
interpStateRead = interpret $ \case
  GetPlayers -> flip constMap () <$> (players <$> get)
  GetVP pl -> sum <$> (fmap getCardVP <$> (join <$> mapM (justGetStack . PlayerCard pl) allPositions))
  GetHand pl -> justGetStack (PlayerCard pl PlayerHand)
  GetDeck pl -> justGetStack (PlayerCard pl PlayerDeck)
  GetTopCard pl -> flip (!?) 1 <$> justGetStack (PlayerCard pl PlayerDeck)
  GetTopNCard pl n -> flip (!?) n <$> justGetStack (PlayerCard pl PlayerDeck)
  GetDiscardPile pl -> justGetStack (PlayerCard pl PlayerDiscardPile)
  IsGameOver -> do
    cards <- activeKingdoms
    emptyPiles <- forM cards emptyStack
    return $ countElem True emptyPiles >= 3
  -- GetReactions pl -> _

isSupply (Supply c) = Just c
isSupply _ = Nothing

getSupplies = mapMaybe isSupply

-- TODO: Fix abstraction barrier broken
-- TODO: Fix maybes everywhere
-- TODO: Surely this is a lens type problem. At least modify??
interpStacks :: Member (State (Map Position [Card])) r => Sem (Stacks : r) a -> Sem r a
interpStacks = interpret $ \case
      ActiveKingdoms -> getSupplies . Map.keys <$> get @(Map Position [Card])
      GetStack loc -> Map.lookup loc <$> get @(Map Position [Card])
      ShuffleStack loc -> do
        cardMap <- get @(Map Position [Card])
        let mstack1 = Map.lookup loc cardMap
        case mstack1 of
            Nothing    -> undefined
            Just stack -> put $ Map.insert loc (undefined stack) cardMap
      StackOnto l1 l2 -> do
        cardMap <- get @(Map Position [Card])
        let mstack1 = Map.lookup l1 cardMap
        let mstack2 = Map.lookup l2 cardMap
        case (mstack1, mstack2) of
            (Just stack1, Just stack2) -> put $ Map.insert l1 [] . Map.insert l2 (stack1++stack2) $ cardMap
            _ -> undefined
      DrawTo l1 l2 -> do
        cardMap <- get @(Map Position [Card])
        let mstack1 = Map.lookup l1 cardMap
        let mstack2 = Map.lookup l2 cardMap
        case (mstack1, mstack2) of
            (Nothing, _)            -> undefined
            (_, Nothing)            -> undefined
            (Just [], _)            -> return Nothing
            (Just (x:xs), Just ys)  -> do
                put $ Map.insert l1 xs . Map.insert l2 (x:ys) $ cardMap
                return $ Just x
      CardToPos card loc -> do
        cardMap <- get @(Map Position [Card])
        let newMap = fmap (filter (card ==)) cardMap -- undefined if this doesn't change it?
        let mstack1 = Map.lookup loc cardMap
        case mstack1 of
            Nothing  -> undefined
            (Just x) -> void $ put $ Map.insert loc (card:x) newMap