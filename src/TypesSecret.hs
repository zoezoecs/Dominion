module TypesSecret (HasReaction, unknownLookupReaction', knownLookupReaction', FaceInfo'(..)) where

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