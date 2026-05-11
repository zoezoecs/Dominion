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

-- EventAnswer f1
blah :: Laws
blah = traversableLaws (Proxy :: Proxy (EventAnswer Maybe))


-- Stacks invariants: Keys unchanged and set of values unchanged. Maintains invariants (shuffle preserves that piles set, stack preserves the append)
-- Randomness: Shuffles are indeed random and repeatable. Check distribution within one seed and across seeds; independence.
-- Logging: logging happens immediately after events
-- Interpreter tests: commutativity, 
-- Game Logic: Probably just integration tests and regression tests.
-- PlayerIO get legal actions returns exactly the legal actions
-- Serialisation: Round trips
-- Traversable laws
-- implicit invariants

-- Regression testing
-- Check basic game rules are upheld properly, property test over all cardfaces and so on
-- Unit tests with throne room
-- Unit tests with game ending and victory
-- No crashes