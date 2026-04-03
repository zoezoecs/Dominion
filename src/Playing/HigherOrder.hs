{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, StandaloneDeriving #-}
module Playing.HigherOrder where

import Polysemy

data MyEffect m a where
  Wah :: Int -> MyEffect m Int
makeSem ''MyEffect

data MyLogEffect m a where
  LogWah :: Int -> MyLogEffect m ()
makeSem ''MyLogEffect

data HigherEffect m a where
  MkHigher :: m a -> HigherEffect m a
makeSem ''HigherEffect

runHigher :: Member MyEffect r => Sem (HigherEffect : r) a -> Sem r a
runHigher = interpretH $ \case
  MkHigher action ->
    -- 1. `runT action` converts the `m a` into a `Sem (HigherEffect : r) a`
    --    that can be recursively interpreted, and gives us a "continuation handler"
    --    `raise . runHigher` to peel off HigherEffect in the recursive call.
    -- 2. We then re-interpret that sub-computation under a local intercept
    --    that increments every `Wah n` to `Wah (n+1)`.
    do
      t <- runT action          -- t :: Sem (HigherEffect : r) a
      raise $ runHigher         -- recursively strip HigherEffect
            $ intercept @MyEffect (\case
                Wah n -> wah (n + 1)  -- increment every Wah by 1
              ) t

-- Interpret MyEffect: Wah n simply returns n
runMyEffect :: Sem (MyEffect : r) a -> Sem r a
runMyEffect = interpret $ \case
  Wah n -> pure n

main :: IO ()
main = do
  let prog1 = runMyEffect $ wah 6
  print (run prog1)  -- 5

  let prog2 = runMyEffect . runHigher $ mkHigher (wah 6)
  print (run prog2)  -- 6
