module TypesSecret (HasReaction, unknownLookupReaction', knownLookupReaction', FaceInfo'(..), InSubset, Subset(..), makeHidden, expose) where

import Data.Maybe

data HasReaction = HasReaction

data FaceInfo' a b c d = FaceInfo {
  getFaceVP' :: a,
  getFaceCurrency' :: Maybe Int,
  getFaceCost' :: Int,
  getFaceTypes' :: [b],
  getFaceReaction' :: Maybe c,
  getFaceEffect' :: Maybe d
}

unknownLookupReaction' :: FaceInfo' a b c d -> Maybe HasReaction
unknownLookupReaction' fi = HasReaction <$ getFaceReaction' fi

knownLookupReaction' :: HasReaction -> FaceInfo' a b c d -> c
knownLookupReaction' HasReaction = fromJust . getFaceReaction'

data InSubset a = InSubset a
data Subset a = Subset (a -> Bool)

-- you can just cook your own subset, we need the type to express this dependence...
makeHidden :: Subset a -> a -> Maybe (InSubset a)
makeHidden (Subset f) a = if f a then Just (InSubset a) else Nothing

expose :: InSubset a -> a
expose (InSubset a) = a