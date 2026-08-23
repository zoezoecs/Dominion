module Main (main) where

import Polysemy

import Effects
import Interpreters

import Data.Aeson
import qualified Data.Aeson as Ae (Result (..))
import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Containers.ListUtils
import qualified Data.Vector.Unboxed as V
import Statistics.Test.ChiSquared (chi2test)
import Statistics.Types
import Statistics.Test.Types
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as QC (run)

import Data.Proxy (Proxy(..))
import Test.QuickCheck.Classes


main :: IO ()
main = putStrLn "Test suite not yet implemented."

runModify :: Monad m => Int -> a -> m (a -> a) -> m a
runModify n a = fmap (($ a) . appEndo . foldMap Endo) . replicateM n

countRandomShuffle  :: (Ord a, Member RandomShuffle r) => Int -> [a] -> Sem r (Map.Map [a] Int)
countRandomShuffle n x = runModify n initial modify
  where
    initial = Map.fromList (map (\z -> (z, 0)) (permutations x))
    modify = fmap (Map.adjust (+1)) (randomShuffle x)

prop_randomShuffleUniform :: (Ord a, Show a, Member RandomShuffle r) => Int -> [a] -> PropertyM (Sem r) Bool
prop_randomShuffleUniform n xs = do
    let perms    = List.permutations xs
        k        = length perms
        expected = fromIntegral n / fromIntegral k :: Double

    pre (k > 1)
    pre (expected > 5)
    counts <- QC.run $ countRandomShuffle n xs

    let
        pairs    = V.fromList [ (Map.findWithDefault 0 p counts, expected) | p <- perms ]
        result   = chi2test 0 pairs

    case result of
        Nothing   -> monitor (label "chi2 inapplicable") >> pure False
        Just test -> do
            let pValue = testSignificance test

            monitor $ counterexample $ unlines
                [ "p-value: " ++ show pValue
                , "observed: " ++ show counts
                , "expected: " ++ show expected
                ]

            pure (pValue > mkPValue 0.05)

uniformShuffle :: Property
uniformShuffle = forAll (choose (0, maxBound :: Int)) $ \seed -> monadic (ioProperty . runM . interpRandomWithSeed seed . interpRandomShuffle) (prop_randomShuffleUniform 500000 [1..4])

shuffleIndependence :: Property
shuffleIndependence = undefined

globalRandomNonDet :: Property
globalRandomNonDet = ioProperty $ fmap ((/= 1) . length . nubOrd) . replicateM 10 . runM @IO . interpRandomGlobal . runRandomUniqueId $ randomUniqueId

-- interpRandomWithSeed is deterministic for a fixed seed
seedRandomDet :: Property
seedRandomDet = forAll (choose (0, maxBound :: Int)) $ \seed -> ioProperty $ do
    val1 <- runM @IO . interpRandomWithSeed seed . runRandomUniqueId $ replicateM 10 randomUniqueId
    val2 <- runM @IO . interpRandomWithSeed seed . runRandomUniqueId $ replicateM 10 randomUniqueId
    pure $ val1 == val2

roundTrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
roundTrip x = Ae.Success x == fromJSON (toJSON x)
-- track down every JSON serialisable datatype?

-- EventAnswer f1
blah :: Laws
blah = traversableLaws (Proxy :: Proxy (EventAnswer Maybe))


-- Effects to test:
--  CardEffects:
--    Unit tests, regression tests
--  Stacks: 
--    Invariants: Keys unchanged, set of values unchanged, shuffle preserves that piles set, stack preserves the append
--    All write actions should change the state?
--    Performance
-- Randomness: 
--   Shuffles are indeed uniform and repeatable. 
--   Cross seed and within seed testing
--   Independence
-- Logging & Information:
--    Logging happens immediately after events
--    Logging logs what actually happened to someone
--    Unit & regression tests for information
--    Unit tests for correlation
-- Interpreter tests: 
--    Commutativity
--    Ordering of emitted effects
-- Game Logic (BoardStateRead, GameLoop, GameRules, actual game loop function):
--    Probably just integration tests, unit tests, regression tests. Make sure encoding issues like invalid players, cards not in supply, etc are covered well.
--    This is probably where all the bugs are going to be, so more attention needs to go here.
-- Reactions:
--    Unit tests, maybe make some extra card faces, check dominion wiki
-- GetValidResponses:
--   QuickCheck with rules
-- Serialisation: Round trips
-- PlayerIO: Not worth testing specifically.

-- Cross effect checks - do writes in one effect and check you can read it in another. Topdeck and then draw, fix the deck and check hand, trash and it leaves the hand, gain vp and getvp goes up, 
-- Create random play functions, since the search space is comparatively small. Can have garbage input strategy, random strategy, vaguely sensible strategy, and switch between them.
-- Criteria for success: No crashes, no exceptions, productive
-- Traversable laws
-- Unit tests:
--   Check Dominion wiki to test edge cases for card logic
--   Sentry etc with one card left in deck but >1 in discard pile
--   Throne room
--   Throne room can't throne room itself
--   Reactions and moat
--   Game ending, order of who can play things

-- How do I check players cant do things they aren't meant to?