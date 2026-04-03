{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, StandaloneDeriving #-}
module Playing.InterpretIntercept where

import Polysemy
import Base

data MyEffect m a where
  Wah :: Int -> MyEffect m Int
makeSem ''MyEffect

data MyLogEffect m a where
  LogWah :: Int -> MyLogEffect m ()
makeSem ''MyLogEffect


testIntercept :: IO Int
testIntercept = runM
     . interpret (\case Wah x -> embed . (>> return x) . putStrLn $ "real" ++ show x)
     . intercept (\case Wah x -> (embed . (>> return x) . putStrLn $ "intercepted3" ++ show x) >> wah x>> wah x)
     . intercept (\case Wah x -> wah x >> (embed . (>> return x) . putStrLn $ "Log Wah!:" ++ show x))
     . intercept (\case Wah x -> (embed . putStrLn $ "intercepted2" ++ show x) >> wah x >> wah x)
     . intercept (\case Wah x -> (embed . putStrLn $ "intercepted1" ++ show x) >> wah x >> wah x)
     $ wah 2

testLogAfter :: IO Int
testLogAfter = runM
              . interpret (\case (LogWah x) -> embed . putStrLn $ "log value:" ++ show x)
              . interpret (\case (Wah x) -> embed . (>> return x) . putStrLn $ "real" ++ show x)
              . intercept (\case (Wah x) -> wah x >>=/ logWah)
               $ wah 2 >> wah 3
