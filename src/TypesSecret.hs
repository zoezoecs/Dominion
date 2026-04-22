module TypesSecret (HasReaction, unknownLookupReaction', knownLookupReaction', FaceInfo'(..)) where

import Data.Maybe

data HasReaction = HasReaction

data FaceInfo' a b c = FaceInfo {
  getFaceVP' :: Int,
  getFaceCurrency' :: Maybe Int,
  getFaceCost' :: Int,
  getFaceTypes' :: [a],
  getFaceReaction' :: Maybe b,
  getFaceEffect' :: Maybe c
}

unknownLookupReaction' :: FaceInfo' a b c -> Maybe HasReaction
unknownLookupReaction' fi = HasReaction <$ getFaceReaction' fi

knownLookupReaction' :: HasReaction -> FaceInfo' a b c -> b
knownLookupReaction' HasReaction = fromJust . getFaceReaction'