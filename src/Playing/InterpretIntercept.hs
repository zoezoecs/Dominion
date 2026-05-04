{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
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
     . interpret (\case Wah x -> embed . (>> pure x) . putStrLn $ "real" ++ show x)
     . intercept (\case Wah x -> (embed . (>> pure x) . putStrLn $ "intercepted3" ++ show x) >> wah x>> wah x)
     . intercept (\case Wah x -> wah x >> (embed . (>> pure x) . putStrLn $ "Log Wah!:" ++ show x))
     . intercept (\case Wah x -> (embed . putStrLn $ "intercepted2" ++ show x) >> wah x >> wah x)
     . intercept (\case Wah x -> (embed . putStrLn $ "intercepted1" ++ show x) >> wah x >> wah x)
     $ wah 2

testLogAfter :: IO Int
testLogAfter = runM
              . interpret (\case (LogWah x) -> embed . putStrLn $ "log value:" ++ show x)
              . interpret (\case (Wah x) -> embed . (>> pure x) . putStrLn $ "real" ++ show x)
              . intercept (\case (Wah x) -> wah x >>=/ logWah)
               $ wah 2 >> wah 3
