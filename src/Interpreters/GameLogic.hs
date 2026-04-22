module Interpreters.GameLogic where

import Polysemy
import Polysemy.State

import Control.Monad
import Data.Maybe
import Data.List

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
      Left err -> return $ Left err
      Right () -> do
        mcard <- gainCard player face
        case mcard of
          Left err -> return $ Left (BadGain err)
          Right card -> do modify (modBuys (-1))
                           return $ Right card
  PlayFromHand player card -> do
    valid_action <- canAct player card
    case valid_action of
      Left err -> return $ Left err
      Right () -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify (modActions (-1))
        activateCard player card
        return $ Right ()
  PlayTreasure player card -> do
    valid_treasure <- canTreasure player card
    case valid_treasure of
      Right n -> do
        cardToPos card (PlayerCard player PlayerInPlay)
        modify $ modCurrency n
        return $ Right n
      Left err -> return $ Left err
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
      Left err -> return . Left $ err
      Right has_reaction -> do
        knownLookupCardReactionM pl card has_reaction
        return . Right $ ()

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
    return result
  CanAct pl card -> do
    gs        <- get @GameState
    handCards <- getHand pl
    let result
          | current_actions gs <= 0                          = Left NoActions
          | CardAction `notElem` (getTypes . getFace) card   = Left NotAnAction
          | card `notElem` handCards                         = Left CardPositionIncorrect
          | otherwise                                        = Right ()
    return result
  CanTreasure pl card -> do
    handCards <- getHand pl
    let result
          | card `notElem` handCards = Left TreasurePositionIncorrect
          | otherwise                = Right ()
    case (result, getCurrency card) of
      (Left err, _) -> return $ Left err
      (Right (), Nothing) -> return $ Left NotATresure
      (Right (), Just n) -> return $ Right n
  CanReact pl card (ReactionEvent (EventAnswer ceff ma)) -> do
    handCards <- getHand pl
    case unknownLookupReaction (getFace card) of
          Nothing -> return $ Left NoReaction
          Just has_reac -> do
            let cond_true = case (knownLookupCond pl card has_reac, ma) of
                      (BeforeReaction cond _, Nothing) -> cond (cardEffectrMap ceff)
                      (AfterReaction cond _, Just a) -> cond (cardEffectrMap ceff) a
                      _ -> False
            let result
                  | card `notElem` handCards = Left NoCard
                  | cond_true                = Left ConditionNotMet
                  | otherwise                = Right has_reac
            return result

gah :: Applicative m => (c -> m a) -> (c -> m b) -> (c -> m (a,b))
gah cma cmb c = liftA2 (,) (cma c) (cmb c)

deidentify :: PotentiallyObscured -> Card
deidentify = undefined

runValidResponses :: Members '[BoardStateRead, Stacks, GameRules] r => InterpreterFor ValidResponses r
runValidResponses = interpret $ \case
  GetValidResponses (GetAction pl) -> do
    handCards <- getHand pl
    cardSuccess <- traverse (gah return (canAct pl)) handCards
    return $ Nothing:[Just c | (c,Right _) <- cardSuccess]
  GetValidResponses (GetPlayTreasure pl) -> do
    handCards <- getHand pl
    cardSuccess <- traverse (gah return (canTreasure pl)) handCards
    return $ Nothing:[Just c | (c,Right _) <- cardSuccess]
  GetValidResponses (GetBuy pl) -> do
    potentialBuys <- activeKingdoms
    cardSuccess <- traverse (gah return (canBuy pl)) potentialBuys
    return $ Nothing:[Just c | (c,Right _) <- cardSuccess]
  GetValidResponses (GetTrashAny _ cards) -> return $ subsequences cards
  GetValidResponses (GetTrashExactlyN _ n cards) -> return $ filter (\x -> length x == n) (subsequences cards)
  GetValidResponses (SendInfo _ _) -> return [()]
  GetValidResponses (GetPlayerReaction pl reac) -> return [Nothing]
  --  do
  --  handCards <- getHand pl
  --  cardSuccess <- traverse (gah return (\c -> canReact pl c (fmap deidentify reac))) handCards
  --  return $ Nothing:[Just c | (c,Right _) <- cardSuccess]

-- TODO: Here we have a potential information leak
-- If a card were to say "if another player draws a Province", they would be able to determine information from that
-- But it seems bad to look up the obscured card.