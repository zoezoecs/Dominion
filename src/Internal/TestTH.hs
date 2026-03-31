{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Internal.TestTH where
import Internal.TH

data Player = Players
data CardFace = CardFace
data PlayerPosition = PlayerPosition
data InvalidGain = InvalidGain
newtype Card = MkCard Int
data MyThing = Either Card Player

data CardEffectss card m a where
  -- Modify game resources
  WahModifyActions :: Int -> CardEffectss card m Int
  WahModifyBuys :: Int -> CardEffectss card m Int
  WahModifyCurrency :: Int -> CardEffectss card m Int

  WahActivateCard :: Player -> card -> CardEffectss card m () -- This just activates a given card with a focus on a player, think Throne Room. 
  -- Note that if you activate a Moat, the card effect depends on the specific card, not just the card face - it will reveal a different card.
  WahDrawOnce :: Player -> CardEffectss card m (Maybe card)  -- Note Maybe signals no cards in draw OR discard
  WahBlockOne :: Player -> card -> CardEffectss card m () -- Blocks the next attack? This could so lead to a bug lmao...
  WahDiscard :: Player -> card -> CardEffectss card m () -- NOTE: None of these are "discard FROM HAND" or anything
  WahTrashCard :: Player -> card -> CardEffectss card m ()
  WahReveal :: Player -> card -> CardEffectss card m ()
  WahTopDeck :: Player -> card -> CardEffectss card m ()
  WahGainCardTo :: Player -> CardFace -> PlayerPosition -> CardEffectss card m (Either InvalidGain card)
makeSemMonomorphised ''Card ''CardEffectss

-- [CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079181], cliEffRes = ConT GHC.Types.Int, cliConName = Internal.TestTH.WahModifyActions, cliFunName = wahModifyActions, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080002,ConT GHC.Types.Int)], cliFunCxt = [], cliUnionName = r_6989586621679080001 },
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079183], cliEffRes = ConT GHC.Types.Int, cliConName = Internal.TestTH.WahModifyBuys, cliFunName = wahModifyBuys, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080004,ConT GHC.Types.Int)], cliFunCxt = [], cliUnionName = r_6989586621679080003},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079185], cliEffRes = ConT GHC.Types.Int, cliConName = Internal.TestTH.WahModifyCurrency, cliFunName = wahModifyCurrency, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080006,ConT GHC.Types.Int)], cliFunCxt = [], cliUnionName = r_6989586621679080005},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079187], cliEffRes = TupleT 0, cliConName = Internal.TestTH.WahActivateCard, cliFunName = wahActivateCard, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080008,ConT Internal.TestTH.Player),(x_6989586621679080009,VarT card_6989586621679079187)], cliFunCxt = [], cliUnionName = r_6989586621679080007},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079189], cliEffRes = AppT (ConT GHC.Maybe.Maybe) (VarT card_6989586621679079189), cliConName = Internal.TestTH.WahDrawOnce, cliFunName = wahDrawOnce, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080011,ConT Internal.TestTH.Player)], cliFunCxt = [], cliUnionName = r_6989586621679080010},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079191], cliEffRes = TupleT 0, cliConName = Internal.TestTH.WahBlockOne, cliFunName = wahBlockOne, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080013,ConT Internal.TestTH.Player),(x_6989586621679080014,VarT card_6989586621679079191)], cliFunCxt = [], cliUnionName = r_6989586621679080012},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079193], cliEffRes = TupleT 0, cliConName = Internal.TestTH.WahDiscard, cliFunName = wahDiscard, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080016,ConT Internal.TestTH.Player),(x_6989586621679080017,VarT card_6989586621679079193)], cliFunCxt = [], cliUnionName = r_6989586621679080015},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079195], cliEffRes = TupleT 0, cliConName = Internal.TestTH.WahTrashCard, cliFunName = wahTrashCard, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080019,ConT Internal.TestTH.Player),(x_6989586621679080020,VarT card_6989586621679079195)], cliFunCxt = [], cliUnionName = r_6989586621679080018},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079197], cliEffRes = TupleT 0, cliConName = Internal.TestTH.WahReveal, cliFunName = wahReveal, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080022,ConT Internal.TestTH.Player),(x_6989586621679080023,VarT card_6989586621679079197)], cliFunCxt = [], cliUnionName = r_6989586621679080021},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079199], cliEffRes = TupleT 0, cliConName = Internal.TestTH.WahTopDeck, cliFunName = wahTopDeck, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080025,ConT Internal.TestTH.Player),(x_6989586621679080026,VarT card_6989586621679079199)], cliFunCxt = [], cliUnionName = r_6989586621679080024},
--   CLInfo {cliEffName = Internal.TestTH.CardEffectss, cliEffArgs = [VarT card_6989586621679079201], cliEffRes = AppT (AppT (ConT Data.Either.Either) (ConT Internal.TestTH.InvalidGain)) (VarT card_6989586621679079201), cliConName = Internal.TestTH.WahGainCardTo, cliFunName = wahGainCardTo, cliFunFixity = Nothing, cliFunArgs = [(x_6989586621679080028,ConT Internal.TestTH.Player),(x_6989586621679080029,ConT Internal.TestTH.CardFace),(x_6989586621679080030,ConT Internal.TestTH.PlayerPosition)], cliFunCxt = [], cliUnionName = r_6989586621679080027}
-- ]
-- CLInfo {
--   cliEffName = Effects.Log, 
--   cliEffArgs = [ConT Effects.Card], 
--   cliEffRes = VarT a_6989586621679090972, 
--   cliConName = Effects.LogEffect, 
--   cliFunName = logEffect, 
--   cliFunFixity = Nothing, 
--   cliFunArgs = [
--     (x_6989586621679092551,AppT (AppT (ConT Effects.Loggable) (VarT card_6989586621679090971)) (VarT a_6989586621679090972)),
--     (x_6989586621679092552,VarT a_6989586621679090972)], 
--   cliFunCxt = [], 
--   cliUnionName = r_6989586621679092550
--   }
