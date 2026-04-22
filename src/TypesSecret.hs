module TypesSecret (HasReaction, unknownLookupReaction', FaceInfo'(..)) where


data HasReaction = HasReaction

data FaceInfo' a b c = FaceInfo {
  getFaceVP' :: Int,
  getFaceCurrency' :: Maybe Int,
  getFaceCost' :: Int,
  getFaceTypes :: [a],
  getReaction :: Maybe b,
  getEffect' :: Maybe c
}

unknownLookupReaction' :: FaceInfo' a b c -> Maybe HasReaction
unknownLookupReaction' fi = HasReaction <$ getReaction fi
