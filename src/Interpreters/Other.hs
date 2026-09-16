module Interpreters.Other where

import Polysemy
import Polysemy.State

import Control.Arrow
import Control.Monad
import Data.Monoid
import Data.Function
import Data.Constraint.Extras
import qualified Data.Map as Map
import Debug.Trace
import Data.List ((\\))

import Base
import Types
import Effects
import Cards
import Interpreters.DoRedact

blockedDefault :: CardEffects' card m a -> a
blockedDefault (ModifyActions {})    = 0        -- or: don't intercept these at all, see note below
blockedDefault (ModifyBuys {})       = 0
blockedDefault (ModifyCurrency {})   = 0
blockedDefault (ActivateCard {})     = ()
blockedDefault (DrawOnce {})         = Nothing
blockedDefault (BlockOne {})         = ()
blockedDefault (Discard {})          = ()
blockedDefault (TrashCard {})        = ()
blockedDefault (Reveal {})           = ()
blockedDefault (TopDeck {})          = ()
blockedDefault (PutInPlay{})         = ()
blockedDefault (GainCardTo {})       = Left GainError
blockedDefault (GetTopDeckN{})       = []

withBlocking
  :: (Members '[Stacks, State GameState, PlayerIO, BoardStateRead, CardEffects] r)
  => Sem r a -> Sem r a
withBlocking action = do
  gs <- get
  let blockedNow = blocks gs
  modify (\g -> g { blocks = Map.map (const False) (blocks g) })
  intercept @CardEffects (\ceff -> case getEffectPlayer ceff of
      Just target | Map.findWithDefault False target blockedNow -> pure (blockedDefault ceff)
      _ -> send (cardEffectrMap ceff)
    ) action

runCardEffectForActivation
  :: (Members '[Stacks, Dispatch, State GameState, PlayerIO, BoardStateRead, CardEffects] r)
  => Card -> Player -> Sem r ()
runCardEffectForActivation c pl
  | isAttack c = withBlocking body
  | otherwise  = body
  where
    body = getEffect (getFace c) pl c

interpCardEffects ::
  (Members '[Stacks, Dispatch, State GameState, PlayerIO, BoardStateRead] r1,
  Members '[Stacks, State GameState, PlayerIO, BoardStateRead] r2) =>
  (forall x. Sem (CardEffects : r1) x -> Sem (CardEffects : r2) x) ->
  Sem (CardEffects : r1) a -> Sem r2 a
interpCardEffects inject = interpCardEffects' . inject
  where
    interpCardEffects' = interpret @CardEffects $ \case
      ModifyActions n -> modify (modActions n) >> current_actions <$> get
      ModifyBuys n -> modify (modBuys n) >> current_buys <$> get
      ModifyCurrency n -> modify (modCurrency n) >> current_currency <$> get
      ActivateCard pl c -> do
        cardToPos c (PlayerCard pl PlayerInPlay)
        interpCardEffects inject (runCardEffectForActivation c pl)
      -- Moat check and reaction checks. Isn't it weird c appears twice? 
      -- Activating cards, even if they aren't by playing from hand, FIRST moves them into play. c.f. Vassal, Throne Room.
      DrawOnce pl -> drawTo (PlayerCard pl PlayerDeck) (PlayerCard pl PlayerHand)
      BlockOne pl _ -> void $ modify (setBlocks pl True)
      Discard pl c -> void $ cardToPos c (PlayerCard pl PlayerDiscardPile)
      TrashCard _ c -> void $ cardToPos c Trash
      Reveal _ _ -> pure () -- Reveal handled elsewhere
      TopDeck pl c -> void $ cardToPos c (PlayerCard pl PlayerDeck)
      PutInPlay pl c -> cardToPos c (PlayerCard pl PlayerInPlay)
      GainCardTo pl c pos -> do
        mcard <- drawTo (Supply c) (PlayerCard pl pos)
        case mcard of
          Nothing -> pure $ Left EmptySupply
          Just card -> pure $ Right card

-- How do I ensure that "If someone gains a treasure" only fires before someone successfully gains a treasure, and not before
-- someone fails to gain a treasure, or after someone gains a treasure?
-- I mean thats not nontrivial, right. What if you have a card that says "when someone gains a card, you also gain a copy"
-- How can that possibly fire before theirs does.
-- Ok, there are two types of reactions, ones that go before and just block, and others that trigger in reaction to events, which
-- go after.
-- Also, I think reactions can only be activated from someones hand. However, many reactions are pureed to the players hand immediately
-- after being put into play.
-- Oh great, and reactions can be chosen by the player of which to play, in player order.
-- We need to recursively check for reaction cards after each one is played, too, to update what can be played

-- We need to circle around each player, ask them for which reactions they want to play while updating the possible reactions,
-- and apply the reactions. This won't quite work with what I've done though - you need to ask the player AFTER the answer is
-- available if they wish to play a valid action. So we need to do two phases, one for before reactions and one for after reactions
-- after the event has occurred

redactReactEvent :: Member Obscure r => ReactionEvent Card -> Player -> Sem r (ReactionEvent PotentiallyObscured)
redactReactEvent ev pl = ReactionEvent <$> redactEvent (getReactionEvent ev) pl

-- Prompt the player to react, Maybe signals choosing to not buy
playOneReaction'
  :: (Members '[DoReaction, PlayerIO, Obscure, GameRules, BoardStateRead] r)
  => Player -> CardEffects (Sem rinitial) a -> Maybe a -> [Card] -> Sem r (Maybe (Card, ())) -> Sem r (Maybe (Card, ()))
playOneReaction' player ceff ma used if_invalid = do
  let realEvent = reactionEvent ceff ma
  hand <- getHand player
  validCards <- filterM (\c -> isRight <$> canReact player c realEvent) (hand \\ used)
  redacted <- redactReactEvent realEvent player
  mreact <- getPlayerReaction player redacted validCards
  case mreact of
    Nothing -> pure Nothing
    Just card -> do
     moutcome <- doReaction player card (reactionEvent ceff ma)
     case moutcome of
      Left _   -> if_invalid
      Right outcome -> pure $ Just (card, outcome)
  where
    isRight (Right _) = True
    isRight (Left _)  = False

playOneReaction :: (Member DoReaction r, Member GameRules r, Member BoardStateRead r, Member PlayerIO r, Member Obscure r) => Player -> CardEffects (Sem rinnitial) a -> Maybe a -> [Card] -> Sem r (Maybe (Card, ()))
playOneReaction pl ceff ma used = fix $ playOneReaction' pl ceff ma used

playerReact :: (Member DoReaction r, Member GameRules r, Member BoardStateRead r, Member PlayerIO r, Member Obscure r) => Player -> CardEffects (Sem rinnitial) a -> Maybe a -> Sem r [()]
playerReact pl ceff ma = go []
  where
    go used = do
      mresult <- playOneReaction pl ceff ma used
      case mresult of
        Nothing            -> pure []
        Just (card, outcome) -> (outcome :) <$> go (card : used)

playerReacts :: Members '[DoReaction, BoardStateRead, GameRules, CardEffects, PlayerIO, Obscure] r => Player -> CardEffects (Sem rinitial) a -> Sem r a
playerReacts player cardEff = do
  _ <- playerReact player cardEff Nothing -- "before reactions"
  ret <- send (cardEffectrMap cardEff)
  _ <- playerReact player cardEff (Just ret) -- "after reactions"
  pure ret

injectReaction :: Members '[BoardStateRead, GameRules, PlayerRoster, PlayerIO, CardEffects, Obscure] r => Sem r a -> Sem (DoReaction:r) a
injectReaction program = do
  players' <- getPlayers
  let players = Map.keys players' -- TODO: This isn't correct, we need the current player's turn (NOT the card effect player). It doesn't matter much for base dominion though.
  let wah x = Endo $ intercept @CardEffects (playerReacts x)
  appEndo (foldMap wah players) (raise program)

calculateVP :: Int -> VictoryPoints -> Int
calculateVP total_cards (VictoryPoints plain gardens) = plain + gardens * div total_cards 10

interpPlayerRoster :: Member (State GameState) r => InterpreterFor PlayerRoster r
interpPlayerRoster = interpret $ \case
  GetPlayers -> flip constMap () <$> (all_players <$> get)

interpStateRead :: Members '[Stacks, State GameState] r => Sem (BoardStateRead : r) a -> Sem r a
interpStateRead = interpret $ \case
  GetVP pl -> do
    playerCards <- join <$> mapM (justGetPlayerStack pl) allPositions
    pure . calculateVP (length playerCards). mconcat . fmap getCardVP $ playerCards
  GetHand pl -> justGetPlayerStack pl PlayerHand
  GetDeck pl -> justGetPlayerStack pl PlayerDeck
  GetDiscardPile pl -> justGetPlayerStack pl PlayerDiscardPile
  GetSupply -> Map.foldrWithKey bah mempty <$> seeStackMap -- If I don't need this to return a Map I can just precomposition
    where
      bah :: Position -> [Card] -> Map.Map CardFace Int -> Map.Map CardFace Int
      bah (Supply cf) y = mappend $ Map.singleton cf (length y)
      bah _ _ = id

runDispatch :: Members '[PlayerRoster, BoardStateRead, State GameState] r => Sem (Dispatch ': r) a -> Sem r a
runDispatch = interpretH $ \case
  ApplyOthers activator f -> do
    allPlayers <- getPlayers
    gs <- get @GameState
    let isBlocked p  = Map.findWithDefault False p (blocks gs)
        others       = filter (/= activator) (Map.keys allPlayers)
        targets      = filter (not . isBlocked) others
        bweh         = mapM (liftToSnd f) targets
    runTSimple (Map.fromList <$> bweh)

  ApplyAll action -> do
    allPlayers <- Map.keys <$> getPlayers
    let    bweh         = mapM (liftToSnd action) allPlayers
    runTSimple (Map.fromList <$> bweh)