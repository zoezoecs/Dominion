module MyLib (main) where

import Polysemy
import Polysemy.Output
import Polysemy.State
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy
 
import Base
import Types
import Data
import Interpreters.Interpreters
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

-- TODO: Separate initialisation and card creation? State init is annoying me...
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
-- Add all cards
-- Implement "Get Valid Moves" "for every PlayerIO prompt"
-- Implement playerIO no actions immediate return
-- Consider partial/failing moves and how that affects things. Atomicity and unnecessary reactions? Relevant for player logging and especially reactions.
-- See if I can fix the effect hierarchy
-- We need interactive state queries lol

-- consider card semantics locations
-- Consider Data formatting json vs haskell

-- Stacks and bad locations
-- Rules validation locations and coverage (c.f. Stacks and CardEffects impossible effect defaulting to signalled ignore)

-- Splitting interpreter logic correctly
-- Commutativity tests
-- Check the thing actually works lmao