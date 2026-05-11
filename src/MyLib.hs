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

injecting :: Members '[GameRules, Log Card, BoardStateRead, PlayerIO, Obscure] r => Sem (CardEffects:r) a -> Sem (CardEffects:r) a
injecting = interpDoReaction . logEffects . injectReaction

main :: [Player] -> [CardFace] -> IO ()
main pl cf = runM .
             serialiseToTerminal .
             -- interpPlayerIO .

             interpRandomWithSeed 4 . -- interpRandomGlobal
             interpRandomShuffle .
             runRandomUniqueId .

             evalState @(Map Position [Card]) (initStacks pl cf) .
             interpStacks (stacksConfig pl).
             evalState @GameState (initGS pl) .
             traceState .
             interpStateRead .

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
             playGame

mainTest :: IO ()
mainTest = main (MkPlayer <$> [1..3]) [Bandit, Moat]

-- TODO: 
-- Correctness bugs, not high priority:
--   Consider partial/failing moves and how that affects things. Atomicity and unnecessary reactions? Relevant for player logging and especially reactions.
--   Fix looking at top n cards with drawing
--   Reactions begin relative to current player
--   Consider rules validation locations and coverage (c.f. Stacks and CardEffects impossible effect defaulting to signalled ignore)
--   Implement scoped for the cards that use it

-- Correctness bugs, high priority
--   Defending against attacks
--   Implement Merchant
--   Possible reactions

-- Testing
--   Chi squared test for randomness
--   Criteria for success: No crashes, no exceptions, productive, no infinite loops
--   Check Dominion wiki to implement edge cases
--   Write other tests

-- Elegance
--   See if I can fix the effect hierarchy (stacks, boardstateread, other things?)
--   Prune useless effect constructors and add useful ones
--   GameRules, ValidResponses, GameLoop, reactions, and logging via intercepting might all be a bit over engineered
--   Is the scoping mechanism really needed in full generality? We could use basically another unique draw thing but idk how to get that to the right place.
--   Splitting interpreter logic correctly
--   Kill partial functions

-- Type security/guarantees/interface security
--   Cards shouldn't get stacks
--   Ensuring we can actually get a gain if we check for it? And making that harder to mess up.
--   Contract expressing the game logic?
--   Stacks and bad locations. Consider making a safer wrapper. Maybe have module based machinery to provide guaranteed accesses.
--   Players.
--   Consider which tests are defining/fundamental/documenting, and which are kind of just thrown in as checks

-- Future work (non essential for testing and playing)
--   We need interactive state queries lol
--   Check card semantics
--   Refactoring for Cards common functionality
--   Current players turn in readstate
--   Disuse \\, union, intersect and hlint it out https://github.com/nh2/haskell-ordnub