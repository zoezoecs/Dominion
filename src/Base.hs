module Base where

import Data.Map (Map)
import qualified Data.Map as Map

dupKey :: Map k () -> Map k k
dupKey = Map.mapWithKey const

constMap :: Ord k => [k] -> a -> Map k a
constMap keys a = Map.fromList $ map (flip (,) a) keys


{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)