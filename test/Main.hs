module Main (main) where

main :: IO ()
main = putStrLn "Test suite not yet implemented."


-- Stacks invariants: Keys unchanged and set of values unchanged. Maintains invariants (shuffle preserves that piles set, stack preserves the append)
-- Randomness: Shuffles are indeed random and repeatable.
-- Logging: logging happens immediately after events
-- Interpreter tests: commutativity, 
-- Game Logic: Probably just integration tests and regression tests.
-- PlayerIO get legal actions returns exactly the legal actions
-- Serialisation: Round trips
-- Traversable laws

-- Regression testing
-- Check basic game rules are upheld properly, property test over all cardfaces and so on
-- Unit tests with throne room
-- Unit tests with game ending and victory
-- No crashes