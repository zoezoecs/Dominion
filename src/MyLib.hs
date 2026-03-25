{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module MyLib (playGame, wah) where


import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Control.Monad.Loops
import Control.Monad
import Data.Function
import Data.Either
import Data.Map (Map)
import Data.Maybe
import Data.List ( (\\) )
import qualified Data.Map as Map


{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n


-- class CardStack f where
--   empty :: f a
--   create :: [a] -> f a
--   destroy :: f a -> [a]
--   insert :: Int -> a -> f a -> f a
--   remove :: Int -> f a -> (Maybe a, f a)
-- 
-- newtype CardStackDefault a = CardStackDefault {getStack :: [a]}
-- 
-- instance CardStack CardStackDefault where
--   empty = CardStackDefault []
--   create = CardStackDefault
--   destroy = getStack
--   insert n a (CardStackDefault cs) = let (xs, ys) = splitAt n cs in CardStackDefault (xs ++ [a] ++ ys)
--   remove n (CardStackDefault cs) = case splitAt n cs of
--     (xs, []) -> (Nothing, CardStackDefault cs)
--     (xs, y:ys) -> (Just y, CardStackDefault $ xs ++ ys)
-- 
-- data AddCard postype cardtype n = AddCard postype cardtype n
-- data RemoveCard postype cardtype n = RemoveCard postype cardtype n
-- data MoveCard from n to m card = Move (AddCard to card n) (RemoveCard from card m)
-- 
-- data CardState ids = None


-- Card state interface which only allows physical manipulations of cards. It will allow for full inspection though.
-- Instance of this which implements dominion specific rules about drawing from empty deck
-- Implemented via some combinators?
--
-- Interface: you can initialise cards into various positions and then move them between positions. You can query for a cards location and query a position. You can draw a specific card out and place it back where it came from, or randomly in a deck.
-- How do we specify how to move them? How do we handle information?
-- Use random number generator, with drawing from top and bottom representing dirac generator?
-- Cards can be in ordered piles or unordered collections

-- Design choice: all cards have ids and aren't just handled as cards.
newtype Card = MkCard Int deriving Eq
data CardFace = Copper | Curse | Estate | Silver | Duchy | Gold | Province |
                Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village |
                Workshop | Bureaucrat | Gardens | Militia |  Moneylender | Poacher |
                Remodel | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory |
                Library | Market | Mine | Sentry | Witch | Artisan  deriving (Eq, Ord)
data CardTypes = CardAttack | CardReaction | CardAction | CardTreasure | CardVictory deriving Eq
newtype Player = MkPlayer Int deriving (Ord, Eq)

-- Obvious design choice: Representing errors and card positions as data
data InvalidMove = NoActions | CardPositionIncorrect
data InvalidBuy = NoMoney | BadGain InvalidGain
data InvalidGain = NotInKingdom | EmptySupply | GainError

data PlayerPosition = PlayerDeck | PlayerDiscardPile | PlayerHand | PlayerInPlay | PlayerSetAside
data Kingdom = Kingdom
data Treasure = Treasure
data CurseSupply = CurseSupplye
data BasicSupply = TreasureSupply | VictorySupply | CurseSupply
-- Design choice: Maybe I just leave Kingdom/Treasure/Blah status to predicates?
-- If I break the card faces up into subsets its annoying to write "Gains a Copper"
-- But if I do this its a little annoying to say "Gain a Treasure"
-- c.f. Gain a treasure costing up to..
data Position = PlayerCard Player PlayerPosition | Supply CardFace | Trash

allPositions :: [PlayerPosition]
allPositions = [PlayerDeck, PlayerDiscardPile, PlayerHand, PlayerInPlay, PlayerSetAside]

-- its not clear why we wouldn't just reinterpret straight into a state monad
data Stacks m a where
  ActiveKingdoms :: Stacks m [CardFace] -- TODO: abstraction barrier broken
  GetStack :: Position -> Stacks m [Card]
  ShuffleStack :: Position -> Stacks m ()
  StackOnto :: Position -> Position -> Stacks m ()
  DrawTo :: Position -> Position -> Stacks m (Maybe Card)
  CardToPos :: Card -> Position -> Stacks m ()
makeSem ''Stacks

data CardEffects m a where
  -- Modify game resources
  ModifyActions :: Int -> CardEffects m Int
  ModifyBuys :: Int -> CardEffects m Int
  ModifyCurrency :: Int -> CardEffects m Int

  ActivateCard :: Player -> Card -> CardEffects m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  DrawOnce :: Player -> CardEffects m (Maybe Card)
  BlockOne :: Player -> CardEffects m () -- Blocks the next attack? This could so lead to a bug lmao...
  Discard :: Player -> Card -> CardEffects m ()
  TrashCard :: Player -> Card -> CardEffects m ()
  Reveal :: Player -> Card -> CardEffects m ()
  TopDeck :: Player -> Card -> CardEffects m ()
  GainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffects m (Either InvalidGain Card)
makeSem ''CardEffects

drawCard :: Member CardEffects r => Player -> Int -> Sem r [Card]
drawCard player n = fmap catMaybes $ replicateM n $ drawOnce player

gainCard :: Member CardEffects r => Player -> CardFace -> Sem r (Either InvalidGain Card)
gainCard pl cf = gainCardTo pl cf PlayerDiscardPile

data Reaction m = Reaction (CardEffects m () -> Bool) (m ())

data BoardStateRead m a where
  GetPlayers :: BoardStateRead m (Map Player ())
  GetVP :: Player -> BoardStateRead m Int
  GetHand :: Player -> BoardStateRead m [Card]
  GetDeck :: Player -> BoardStateRead m [Card]
  GetTopCard :: Player -> BoardStateRead m (Maybe Card)
  GetTopNCard :: Player -> Int -> BoardStateRead m (Maybe Card)
  GetDiscardPile :: Player -> BoardStateRead m [Card]
  IsGameOver :: BoardStateRead m Bool
  -- IsValidCardPlay :: Player -> Card -> BoardStateRead m (Either InvalidMove ())
  -- GetReactions :: Player -> BoardStateRead m [Reaction m]
makeSem ''BoardStateRead

data BoardStateEdit m a where
  StartingResources :: Player -> BoardStateEdit m ()
  BuyCard :: Player -> CardFace -> BoardStateEdit m (Either InvalidBuy Card)
  PlayFromHand :: Player -> Card -> BoardStateEdit m (Either InvalidMove ()) -- This is what should be used to check actions and membership in hand
  -- Design choice: Inline recovery function. c.f. Error, Either, Validation/token checking, state versioning, linearity, uuids
  DrawTurnStart :: Player -> Int -> BoardStateEdit m [Card] -- Draw from deck
  DiscardHandCleanup :: Player -> BoardStateEdit m ()
makeSem ''BoardStateEdit

data Log m a where
  LogPlayerRoundStart :: Player -> Log m ()
  LogBuy :: Player -> CardFace -> Log m Card
  LogAct :: Player -> Card -> Log m ()
  LogDraw :: Player -> Log m Card -- Remember some players wont get a log message with the card drawn.
  LogDiscard :: Player -> Log m Card
  LogReveal :: Player -> Card -> Log m Card
makeSem ''Log

data BoardInit m a where
  SetSupply :: Map CardFace Int -> BoardInit m ()
  SetHand :: Map CardFace Int -> BoardInit m () -- NOTE: DOES NOT INCLUDE COPPER? COPPER IS DRAWN FROM THE TOTAL, ESTATES ARENT.
makeSem ''BoardInit

-- Obvious design choice: Separate player IO and clients out from server/central logic.
data PlayerIO m a where
  GetAction :: Player -> PlayerIO m (Maybe Card)
  GetBuy :: Player -> PlayerIO m (Maybe CardFace)
  GetTrashAny :: Player -> [Card] -> PlayerIO m [Card]
  GetTrashExactlyN :: Player -> Int -> [Card] -> PlayerIO m [a]
  SendInfo :: Player -> PlayerIO m ()
makeSem ''PlayerIO

type CardSemantics = forall r. Members [BoardStateRead, CardEffects, PlayerIO] r => Player -> Card -> Sem r ()

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

-- type CardSemantics = forall r. Members [BoardStateRead, CardEffects, PlayerIO] r => Player -> Card -> Sem r ()
-- TODO: Make this an effect?
getTypes :: CardFace -> [CardTypes]
getTypes = undefined
getReaction :: CardFace -> (Reaction m)
getReaction = undefined
getFace :: Card -> CardFace
getFace = undefined
getCardVP :: Card -> Int
getCardVP = undefined
getEffect :: CardFace -> CardSemantics
getEffect = undefined

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


countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)


interpStateRead :: Members '[Stacks, State GameState, Log] r => Sem (BoardStateRead : r) a -> Sem r a
interpStateRead = interpret $ \case
  GetPlayers -> flip constMap () <$> (players <$> get)
  GetVP pl -> sum <$> (fmap getCardVP <$> (join <$> mapM (getStack . PlayerCard pl) allPositions))
  GetHand pl -> getStack (PlayerCard pl PlayerHand)
  GetDeck pl -> getStack (PlayerCard pl PlayerDeck)
  GetTopCard pl -> flip (!?) 1 <$> getStack (PlayerCard pl PlayerDeck)
  GetTopNCard pl n -> flip (!?) n <$> getStack (PlayerCard pl PlayerDeck)
  GetDiscardPile pl -> getStack (PlayerCard pl PlayerDiscardPile)
  IsGameOver -> do
    cards <- activeKingdoms
    emptyPiles <- forM cards emptyStack
    return $ countElem True emptyPiles >= 3
  -- GetReactions pl -> _

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
          Left _ -> return $ Left (BadGain GainError)
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

-- Design choice: Effects that can be easily written as a composition are still separate effects that are just reinterpreted with the composition?
-- Design choice: Messages to clients entirely through separate messages, and logs are reinterpreted from effects
-- Partial information managed by different messages with less information, no state tracking, no ability to refer to
-- previous messages for granular information (the card that was drawn 2 turns ago was a ...)
-- Clients don't reconstruct state, they just display the required information and collect the moves.
-- Clients don't see the card logic causality, they just see streams of events and must infer themselves.
-- How to make sure enough information gets through? We need a protocol.

-- Design choice: Reaction effects checked once per type of trigger, at the interpretation of the triggerable event.
-- Since most reaction effects are due to the same few triggers, its not worth implementing a trigger system that checks every effect and 
-- potentially arbitrarily `intercept`s the interpretation
-- That would support some arbitrary intercepting rules modifying gameplay, which is unnecessarily complex code permitted by the types.
-- Instead, we are going to hard code a mechanism for blocking attacks, and place several places where reactions will be checked for.
-- Possibilities:
-- 1. Listeners and event emitters
-- 2. Conditions and effects check for conditions/reactions via an (event/boolean check)
-- 3. ...adding checks in the state, I guess.
-- 4. Rule combinators/overriding/biased monoids
-- 5. Emit an event for an attack into a big events datatype


-- Mechanics:
-- "First time"
-- Cost reduction
-- Overpay
-- Extra turns - Possession
-- Haggler, Talisman, Royal Seal changes each buy
-- Contraband/Embargo gives buying restrictions or penalties
-- Cavalry/Villa: Buy phase back to action


dupKey :: Map k () -> Map k k
dupKey = Map.mapWithKey const

applyTo :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
applyTo f xs = mapM f =<< xs

applyToOthers :: (Member CardEffects r, Member BoardStateRead r) => Player -> (Player -> Sem r a) -> Sem r (Map Player a)
applyToOthers player f = applyTo f (dupKey . Map.delete player <$> getPlayers)

discardHand' :: (Member CardEffects r, Member BoardStateRead r) => Player -> Sem r ()
discardHand' player = void $ applyTo (discard player) (getHand player)

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

constMap :: Ord k => [k] -> a -> Map k a
constMap keys a = Map.fromList $ map (flip (,) a) keys

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

initGS :: [Player] -> GameState
initGS players = MkGameState {players = players,
  blocks = constMap players False,
  current_player = minimum players,
  current_actions = 0,
  current_buys = 0,
  current_currency = 0
  -- reactions :: [Reaction m]
}

wah :: Members '[BoardInit, PlayerIO, Stacks, Log] r => [Player] -> [CardFace] -> Sem r ()
wah pl cf =  evalState @GameState (initGS pl) .
             interpStateRead .
             interpCardEffects .
             interpStateWrite $ playGame pl cf

-- TODO: Log interception, reactions, add all cards, separate code better into files.



--bandit :: (Member BoardStateRead r, Member CardEffects r, Member PlayerIO r) => Player -> Sem r ()
bandit :: CardSemantics
bandit player _ = do
  gainCard player Gold
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
  drawCard player 1
  modifyActions 1
  applyToOthers player (`gainCard` Curse)
  return ()

-- moatPlay :: (Member CardEffects r) => Player -> Sem r ()
moatPlay :: CardSemantics
moatPlay player _ = void $ drawCard player 2

isAttack :: CardFace -> Bool
isAttack face = CardAttack `elem` getTypes face

otherPlayerAttack :: Player -> CardEffects r a -> Bool
otherPlayerAttack player (ActivateCard pl card) = (player /= pl) && isAttack (getFace card)
otherPlayerAttack _ _ = False

moatReact :: (Member CardEffects r) => Player -> Card -> Reaction (Sem r)
moatReact player card = Reaction (otherPlayerAttack player) moatBlock
  where
    moatBlock = do
      reveal player card
      blockOne player

--councilRoom :: (Member CardEffects r) => Player -> Sem r ()
councilRoom :: CardSemantics
councilRoom player _ = do
  drawCard player 4
  modifyBuys 1
  applyToOthers player (`drawCard` 1)
  return ()