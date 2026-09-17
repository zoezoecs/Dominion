module MyLib (main, mainTest) where

import Polysemy
--import Polysemy.Output
import Polysemy.State
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
--import Data.ByteString.Lazy
 
import Base
import Types
import Data
import Interpreters
import Effects
import GameLoop
import Debug.Trace

-- Missing tricky mechanics:
-- "First time"
-- Cost reduction
-- Overpay
-- Extra turns - Possession
-- Haggler, Talisman, Royal Seal changes each buy
-- Contraband/Embargo gives buying restrictions or penalties
-- Cavalry/Villa: Buy phase back to action

initGS :: [Player] -> GameState
initGS players = MkGameState {all_players = players,
  blocks = constMap players False,
  current_actions = 0,
  current_buys = 0,
  current_currency = 0
  -- reactions :: [Reaction m]
}

stacksConfig :: [Player] -> PileConfig []
stacksConfig players = PileConfig { 
                 refillFrom = Map.fromList [(PlayerCard pl PlayerDeck, PlayerCard pl PlayerDiscardPile) | pl <- players ],
                 shuffleOnRefill = [PlayerCard pl PlayerDeck | pl <- players]
                 }

getFresh :: Member (State Int) r => Sem r Int
getFresh = modify (+(1::Int)) >> get

createCard :: Member (State Int) r => CardFace -> Sem r Card
createCard cf = flip MkCard cf <$> getFresh

createCards' :: Member (State Int) r => (CardFace, Int) -> Sem r [Card]
createCards' (cf, n) = replicateM n (createCard cf)

createCards'' :: Member (State Int) r => [(CardFace, Int)] -> Sem r [Card]
createCards'' xs = join <$> mapM createCards' xs

createCards :: Member (State Int) r => Map Position [(CardFace, Int)] -> Sem r (Map Position [Card])
createCards = mapM createCards''

initStacks :: [Player] -> [CardFace] -> Map Position [Card]
initStacks pl cf = run . evalState @Int 0 . createCards $ boardInitState pl cf

traceState :: (Member (State (GameState)) r) => Sem r a -> Sem r a
traceState = intercept @(State GameState) $ \case
  Get -> get
  Put x -> put $ traceShowId x

injecting :: Members '[GameRules, Log Card, PlayerRoster, BoardStateRead, PlayerIO, Obscure] r => Sem (CardEffects:r) a -> Sem (CardEffects:r) a
injecting = logEffects . interpDoReaction . injectReaction

main_ :: Bool -> Int -> PileConfig [] -> Map Position [Card] -> GameState -> IO ()
main_ initGame randSeed pileconfig0 stack0 gs0 = runM .
             serialiseToTerminal .
             -- interpPlayerIO .

             interpRandomWithSeed randSeed . -- interpRandomGlobal
             interpRandomShuffle .
             runRandomUniqueId .

             evalState @(Map Position [Card]) stack0 .
             interpStacks pileconfig0 .
             evalState @GameState gs0 .
             traceState .
             interpStateRead .
             interpPlayerRoster .
             runDispatch False .

             -- runOutputList .
             runCorrelation . 
             interpGameRules .
             runValidResponses .

             interpPlayerIOChoice .
             logPlayerToPlayerIO . 
             -- logPlayerToString @PotentiallyObscured .
             logToPlayerLog .

             interpCardEffects injecting. -- TODO: Check that the reaction to reaction semantics are correct
             interpGameLoop .
             logTurn 
             $
             when initGame setInitialGameState >>
             playGame

main :: [Player] -> [CardFace] -> IO ()
main pl cf = main_ True 4 (stacksConfig pl) (initStacks pl cf) (initGS pl)

mainTest :: IO ()
mainTest = main_ True 4 (stacksConfig pl) (initStacks pl cf) (initGS pl)
  where
    n = 3
    pl = MkPlayer <$> [1..n]
    cf = [Bandit, Moat, ThroneRoom, Village, Militia, Vassal, Sentry, Mine]

moatTest :: IO ()
moatTest = main_ False 4 (stacksConfig pl) stacks0 (initGS pl)
  where
    n = 3
    pl@[p1, p2, p3] = MkPlayer <$> [1..n]
    cf = [Bandit, Moat, ThroneRoom, Village, Militia, Vassal, Sentry, Mine]
    hand1 = [MkCard 9001 Militia, MkCard 9002 Copper, MkCard 9003 Copper, MkCard 9004 Copper, MkCard 9005 Copper]
    hand2 = [MkCard 9006 Moat,    MkCard 9007 Copper, MkCard 9008 Copper, MkCard 9009 Copper, MkCard 9010 Copper]
    hand3 = [MkCard 9011 Copper,  MkCard 9012 Copper, MkCard 9013 Copper, MkCard 9014 Copper, MkCard 9015 Copper]
    stacks0 = Map.insert (PlayerCard p1 PlayerHand) hand1
            . Map.insert (PlayerCard p2 PlayerHand) hand2
            . Map.insert (PlayerCard p3 PlayerHand) hand3
            $ initStacks pl cf

-- Tests to write:
-- Moat works (players dont get attacked, dont get prompted)
-- Sentry works (if one card in deck, big discard, then they draw 2 cards)
-- Information redacting tests

-- fix stacks location stuff

-- TODO: 
-- Correctness bugs, not high priority:
--   Consider partial/failing moves and how that affects things. Atomicity and unnecessary reactions? Relevant for player logging and especially reactions.
--   Reactions begin relative to current player
--   Consider rules validation locations and coverage (c.f. Stacks and CardEffects impossible effect defaulting to signalled ignore)
--   Implement scoped for the cards that use it
--   Really any interpreters should not be using each other. The blocking mechanism is bad for this, and so is Stacks calling itself unnecessarily

-- Correctness bugs, high priority
--   Implement Merchant

-- Elegance
--   Prune useless effect constructors and add useful ones
--   GameRules, ValidResponses, GameLoop, reactions, scoping, and logging via intercepting might all be a bit over engineered
--   Splitting interpreter logic correctly
--   Kill partial functions

-- Type security/guarantees/interface security
--   Ensuring we can actually get a gain if we check for it? And making that harder to mess up.
--   Contract expressing the game logic?
--   Consider which tests are defining/fundamental/documenting, and which are kind of just thrown in as checks

-- Future work (non essential for testing and playing)
--   We need interactive state queries lol
--   Check card semantics
--   Refactoring for Cards common functionality
--   Current players turn in readstate
--   Disuse \\, union, intersect and hlint it out https://github.com/nh2/haskell-ordnub