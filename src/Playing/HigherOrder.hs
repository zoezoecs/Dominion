{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Playing.HigherOrder where

import Polysemy
import Polysemy.State
import Polysemy.Scoped
import Control.Monad
import Debug.Trace
import Polysemy.Opaque


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

runMyEffectPrint :: Member (Embed IO) r => Sem (MyEffect : r) a -> Sem r a
runMyEffectPrint = interpret $ \case
  Wah n -> embed (putStrLn ("Wah called with: " ++ show n)) >> pure n

mainMy :: IO ()
mainMy = do
  let prog1 = runMyEffect $ wah 6
  print (run prog1)  -- 6

  let prog2 = runMyEffect . runHigher $ mkHigher (wah 6)
  print (run prog2)  -- 7

-- Runs the sub-computation, and if the result satisfies the predicate,
-- runs it again, returning the last result either way.
data Retry m a where
  RetryIf :: (a -> Bool) -> m a -> Retry m a
makeSem ''Retry

runRetry :: Member MyEffect r => Sem (Retry : r) a -> Sem r a
runRetry = interpretH $ \case
  RetryIf p action -> do
    inspector <- getInspectorT
    t1 <- runT action
    r1 <- raise $ runRetry t1
    case inspect inspector r1 of
      Just a | p a -> do
        t2 <- runT action
        raise $ runRetry t2
      _ ->
        pure r1

mainRetry :: IO ()
mainRetry = do
  -- wah returns its argument; retry if result < 5
  -- wah 3 returns 3, which is < 5, so we run again
  -- but it's a pure computation so we get 3 again
  let prog = runMyEffectPrint . runRetry $ retryIf (< 5) (wah 3 >> wah 7)
  runM @IO prog  -- 3 (retried but same result, that's fine)

  -- More interesting with a stateful effect like State
  -- but with just MyEffect, the point is runT called conditionally
  let prog2 = runMyEffect . runRetry $ retryIf (< 5) (wah 10)
  print (run prog2)  -- 10 (no retry needed)
  


data Capped m a where
  WithCap :: Int -> m a -> Capped m a

makeSem ''Capped

runCapped :: Member MyEffect r => Sem (Capped : r) a -> Sem r a
runCapped = interpretH $ \case
  WithCap cap action -> do
    t <- runT action
    raise
      . runCapped
      . intercept @MyEffect (\case
          Wah n -> wah (min n cap)
        )
      $ t

mainCapped :: IO ()
mainCapped = do
  result <- runM $ runMyEffect . runCapped $ withCap 5 (wah 10)
  print result   -- prints "Wah called with: 5", returns 5

  result2 <- runM $ runMyEffect . runCapped $ withCap 5 (wah 3)
  print result2  -- prints "Wah called with: 3", returns 3


data Wrap m a where
  Wrap :: m a -> Wrap m a

makeSem ''Wrap

runWrap :: Member (Embed IO) r => Sem (Wrap : r) a -> Sem r a
runWrap = interpretH $ \case
  Wrap action -> do
    embed $ putStrLn "before"
    t <- runT action
    embed $ putStrLn "still before"
    r <- raise $ runWrap t
    embed $ putStrLn "after"
    pure r

mainWrap :: IO ()
mainWrap = runM $ runWrap $ do
  wrap $ embed $ putStrLn "inside"

mainWrap2 :: IO ()
mainWrap2 = runM $ runWrap $ do
  wrap $ embed $ putStrLn "inside"
  embed $ putStrLn "after wrap call"  -- this is also inside t

mainWrapWrap :: IO ()
mainWrapWrap = runM $ runWrap $ do
  wrap $ wrap $ embed $ putStrLn "inside"

-- TODO: Example with raise and duplicate effects

data Counter m a where
  Count :: Counter m Int
makeSem ''Counter

data GroupCounter m a where
  Group :: m a -> GroupCounter m a
makeSem ''GroupCounter

runCounter :: Members '[Embed IO] r => Sem (Counter : r) a -> Sem (State Int : r) a
runCounter = reinterpret $ \case
  Count -> do
    n <- get
    put $ n+1
    embed $ print n
    pure $ n+1

runGroupCounter :: Members '[Embed IO] r => Sem (GroupCounter : r) a -> Sem r a
runGroupCounter = interpretH $ \case
  Group m -> do
    calc' <- runT m
    --let calc = raise @Counter . raise @(State Int) $ calc'
    let calc = subsume_ calc'
    (n, blah) <- raise . runGroupCounter . runState (2::Int) . runCounter $ calc
    pure blah

mockState :: s -> Sem (State s : r) a -> Sem r a
mockState s = interpret $ \case
  Get -> pure s
  Put _ -> pure ()

mainCounter :: IO Int
mainCounter = runM . 
              mockState (0::Int) . 
              runCounter . 
              runGroupCounter $ 
              program
  where
    program :: Members '[GroupCounter, Counter] r => Sem r Int
    program = do
      count
      count
      group (replicateM 3 count)
      group (replicateM 2 (group count))
      group (replicateM 4 count)
      count

mainCounterScoped :: IO Int
mainCounterScoped = runM . 
              mockState (0::Int) . 
              runCounter . 
              runScopedNew @() @Counter (const $ evalState (0::Int) . subsume_ . runCounter) $  -- If I change subsume_ to raise_ or raise . raise the semantics changes...
              program
  where
    program :: Members '[Scoped_ Counter, Counter] r => Sem r Int
    program = do
      count -- 0
      count -- 0
      scoped_ @Counter (replicateM 3 count) -- 0 1 2
      scoped_ @Counter (replicateM 2 (scoped_ @Counter count)) -- 0 0
      scoped_ @Counter (replicateM 4 count) -- 0 1 2 3 
      count -- 0