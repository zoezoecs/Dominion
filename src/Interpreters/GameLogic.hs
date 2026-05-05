module Interpreters.GameLogic where

import Polysemy
import Polysemy.State

import Control.Monad
import Data.Maybe
import Data.List

import Base
import Types
import Effects
import Cards

interpGameLoop :: Members '[Stacks, State GameState, BoardStateRead, GameRules, CardEffects] r => Sem (GameLoop : r) a -> Sem r a
interpGameLoop = interpret $ \case
  StartingResources _ -> do -- Starting player is implicit in game state
    modify (\gs -> gs
      { current_actions  = 1
      , current_buys     = 1
      , current_currency = 0
      })
  BuyCard player face -> do
    valid_buy <- canBuy player face
    case valid_buy of
      Left err -> pure $ Left err
      Right () -> do
        mcard <- gainCard player face
        case mcard of
          Left err -> pure $ Left (BadGain err)
          Right card -> do modify (modBuys (-1))
                           pure $ Right card
  PlayFromHand player card -> do
    valid_action <- canAct player card
    case valid_action of
      Left err -> pure $ Left err
      Right () -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify (modActions (-1))
        activateCard player card
        pure $ Right ()
  PlayTreasure player card -> do
    valid_treasure <- canTreasure player card
    case valid_treasure of
      Right n -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify $ modCurrency n
        pure $ Right n
      Left err -> pure $ Left err
  DrawTurnStart pl n -> drawCard pl n
  DiscardHandCleanup pl -> do
    hand <- getHand pl
    forM_ hand (discard pl)
    stackOnto (PlayerCard pl PlayerInPlay) (PlayerCard pl PlayerDiscardPile)
    stackOnto (PlayerCard pl PlayerSetAside) (PlayerCard pl PlayerDiscardPile)

interpDoReaction :: Members '[GameRules, CardEffects] r => Sem (DoReaction : r) a -> Sem r a
interpDoReaction = interpret $ \case
  DoReaction pl card reac -> do
    valid_reaction <- canReact pl card reac
    case valid_reaction of
      Left err -> pure . Left $ err
      Right has_reaction -> do
        knownLookupCardReactionM pl card has_reaction
        pure . Right $ ()

interpGameRules :: Members '[State GameState, Stacks, BoardStateRead] r => Sem (GameRules : r) a -> Sem r a
interpGameRules = interpret $ \case
  CanBuy _ face -> do
    gs        <- get @GameState
    stack     <- getStack (Supply face)
    let result
          | current_buys gs <= 0                    = Left NoBuys
          | current_currency gs < getFaceCost face  = Left NoMoney
          | isNothing stack                         = Left $ BadGain NotInKingdom
          | stack == Just []                        = Left $ BadGain EmptySupply
          | otherwise                               = Right ()
    pure result
  CanAct pl card -> do
    gs        <- get @GameState
    handCards <- getHand pl
    let result
          | current_actions gs <= 0              = Left NoActions
          | CardAction `notElem` getTypes card   = Left NotAnAction
          | card `notElem` handCards             = Left CardPositionIncorrect
          | otherwise                            = Right ()
    pure result
  CanTreasure pl card -> do
    handCards <- getHand pl
    let result
          | card `notElem` handCards = Left TreasurePositionIncorrect
          | otherwise                = Right ()
    case (result, getCurrency card) of
      (Left err, _) -> pure $ Left err
      (Right (), Nothing) -> pure $ Left NotATresure
      (Right (), Just n) -> pure $ Right n
  CanReact pl card (ReactionEvent (EventAnswer ceff ma)) -> do
    handCards <- getHand pl
    case unknownLookupReaction (getFace card) of
          Nothing -> pure $ Left NoReaction
          Just has_reac -> do
            let cond_true = case (knownLookupCond pl card has_reac, ma) of
                      (BeforeReaction cond _, Nothing) -> cond (cardEffectrMap ceff)
                      (AfterReaction cond _, Just a) -> cond (cardEffectrMap ceff) a
                      _ -> False
            let result
                  | card `notElem` handCards = Left NoCard
                  | cond_true                = Left ConditionNotMet
                  | otherwise                = Right has_reac
            pure result

deidentify :: PotentiallyObscured -> Card
deidentify = undefined

runValidResponses :: Members '[BoardStateRead, Stacks, GameRules] r => InterpreterFor ValidResponses r
runValidResponses = interpret $ \case
  GetValidResponses (GetAction pl) -> do
    handCards <- getHand pl
    cardSuccess <- traverse (fanout pure (canAct pl)) handCards
    pure $ Nothing:[Just c | (c,Right _) <- cardSuccess]
  GetValidResponses (GetPlayTreasure pl) -> do
    handCards <- getHand pl
    cardSuccess <- traverse (fanout pure (canTreasure pl)) handCards
    pure $ Nothing:[Just c | (c,Right _) <- cardSuccess]
  GetValidResponses (GetBuy pl) -> do
    potentialBuys <- activeSupplies
    cardSuccess <- traverse (fanout pure (canBuy pl)) potentialBuys
    pure $ Nothing:[Just c | (c,Right _) <- cardSuccess]
  -- GetValidResponses (GetTrashAny _ cards) -> pure $ subsequences cards
  -- GetValidResponses (GetTrashExactlyN _ n cards) -> pure $ filter (\x -> length x == n) (subsequences cards)
  GetValidResponses (SendInfo _ _) -> pure [()]
  GetValidResponses (GetPlayerReaction _ _) -> pure [Nothing] -- TODO: Fix
  GetValidResponses (GetCardTEMP _ cards) -> pure cards
  GetValidResponses (GetCardsTEMP _ cards) -> pure $ subsequences cards
  GetValidResponses (GetMCardTEMP _ cards) -> pure $ Nothing:(Just <$> cards)
  GetValidResponses (GetNCardsTEMP _ n cards) -> pure $ filter ((==n) . length) (subsequences cards)
  GetValidResponses (GetUpToNCardsTEMP _ n cards) -> pure $ filter ((<=n) . length) (subsequences cards)
  GetValidResponses (SendStack _ _) -> pure [()]
  GetValidResponses (GetCardFaceTEMP _ faces) -> pure faces
  --  do
  --  handCards <- getHand pl
  --  cardSuccess <- traverse (gah pure (\c -> canReact pl c (fmap deidentify reac))) handCards
  --  pure $ Nothing:[Just c | (c,Right _) <- cardSuccess]

-- TODO: Here we have a potential information leak
-- If a card were to say "if another player draws a Province", they would be able to determine information from that
-- But it seems bad to look up the obscured card.