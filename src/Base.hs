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

bind2 :: Monad m => m (Either a b) -> (a -> m r) -> (b -> m r) -> m r
bind2 m fa fb = m >>= either fa fb

bindRight :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a c)
bindRight m k = bind2 m (pure . Left) (fmap Right . k)

bindRight' :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a b)
bindRight' m k = bind2 m (pure . Left) (\x -> k x >> return (Right x))

ifSuccess :: Monad m => m (Either a b) -> m c -> m (Either a b)
ifSuccess a b = bindRight' a (const b)

ifSuccessMaybe :: Monad m => m (Maybe b) -> (b -> m c) -> m (Maybe b)
ifSuccessMaybe mmb mc = do
    mb <- mmb
    case mb of
        Nothing -> return Nothing
        Just x -> mc x >> return (Just x)
    
(>>=/) :: Monad m => m a -> (a -> m b) -> m a
(>>=/) ma f = do
    x <- ma
    _ <- f x
    return x