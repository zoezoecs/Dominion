module Interpreters.Other where

import Polysemy
import Polysemy.State

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Types
import Effects
import Cards


interpCardEffects :: Members '[Stacks, State GameState, PlayerIO, BoardStateRead] r => Sem (CardEffects : r) a -> Sem r a
interpCardEffects = interpret $ \case
  ModifyActions n -> modify (modActions n) >> current_actions <$> get
  ModifyBuys n -> modify (modBuys n) >> current_buys <$> get
  ModifyCurrency n -> modify (modCurrency n) >> current_currency <$> get
  ActivateCard pl c -> interpCardEffects (getEffect (getFace c) pl c) -- Moat check and reaction checks. Isn't it weird c appears twice?
  DrawOnce pl -> drawTo (PlayerCard pl PlayerDeck) (PlayerCard pl PlayerHand)
  BlockOne pl _ -> void $ modify (setBlocks pl True)
  Discard pl c -> void $ cardToPos c (PlayerCard pl PlayerDiscardPile)
  TrashCard _ c -> void $ cardToPos c Trash
  Reveal _ _ -> return () -- Reveal handled elsewhere
  TopDeck pl c -> void $ cardToPos c (PlayerCard pl PlayerDeck)
  GainCardTo pl c pos -> do
    mcard <- drawTo (Supply c) (PlayerCard pl pos)
    case mcard of
      Nothing -> return $ Left EmptySupply
      Just card -> return $ Right card

-- via interceptH as well to check then?
-- cleanup?
interpReaction :: Sem (Reaction : r) a -> Sem r a
interpReaction = interpretH $ \case
  Reaction cond m -> undefined

-- TODO: Fix this
justGetStack :: Member Stacks r => Position -> Sem r [Card]
justGetStack p = do
    mstack <- getStack p
    case mstack of
        Nothing -> undefined
        Just cards -> return cards

interpStateRead :: Members '[Stacks, State GameState] r => Sem (BoardStateRead : r) a -> Sem r a
interpStateRead = interpret $ \case
  GetPlayers -> flip constMap () <$> (all_players <$> get)
  GetVP pl -> sum <$> (fmap getCardVP <$> (join <$> mapM (justGetStack . PlayerCard pl) allPositions))
  GetHand pl -> justGetStack (PlayerCard pl PlayerHand)
  GetDeck pl -> justGetStack (PlayerCard pl PlayerDeck)
  GetTopCard pl -> flip (!?) 1 <$> justGetStack (PlayerCard pl PlayerDeck)
  GetTopNCard pl n -> flip (!?) n <$> justGetStack (PlayerCard pl PlayerDeck)
  GetDiscardPile pl -> justGetStack (PlayerCard pl PlayerDiscardPile)
  IsGameOver -> do
    cards <- activeKingdoms
    emptyPiles <- forM cards (\face -> null <$> getStack (Supply face))
    provinces <- justGetStack (Supply Province)
    return $ null provinces || countElem True emptyPiles >= 3
  -- GetReactions pl -> _

interpCorrelation :: Members '[Obscure, LogToPlayer (Either Card ObscuredCard)] r => Sem (Correlation : r) a -> Sem r a
interpCorrelation = undefined

interpPlayerIO :: Member (Embed IO) r => Sem (PlayerIO : r) a -> Sem r a
interpPlayerIO = undefined