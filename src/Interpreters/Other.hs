module Interpreters.Other where

import Polysemy
import Polysemy.State

import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Data.Function
import Data.Constraint.Extras
import qualified Data.Map as Map
import Debug.Trace

import Base
import Types
import Effects
import Cards
import Interpreters.DoRedact


interpCardEffects ::
  (Members '[Stacks, State GameState, PlayerIO, BoardStateRead] r1,
  Members '[Stacks, State GameState, PlayerIO, BoardStateRead] r2) =>
  (forall x. Sem (CardEffects : r1) x -> Sem (CardEffects : r2) x) ->
  Sem (CardEffects : r1) a -> Sem r2 a
interpCardEffects inject = interpCardEffects' . inject
  where
    interpCardEffects' = interpret @CardEffects $ \case
      ModifyActions n -> modify (modActions n) >> current_actions <$> get
      ModifyBuys n -> modify (modBuys n) >> current_buys <$> get
      ModifyCurrency n -> modify (modCurrency n) >> current_currency <$> get
      ActivateCard pl c -> interpCardEffects inject (getEffect (getFace c) pl c) -- Moat check and reaction checks. Isn't it weird c appears twice?
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

-- How do I ensure that "If someone gains a treasure" only fires before someone successfully gains a treasure, and not before
-- someone fails to gain a treasure, or after someone gains a treasure?
-- I mean thats not nontrivial, right. What if you have a card that says "when someone gains a card, you also gain a copy"
-- How can that possibly fire before theirs does.
-- Ok, there are two types of reactions, ones that go before and just block, and others that trigger in reaction to events, which
-- go after.
-- Also, I think reactions can only be activated from someones hand. However, many reactions are returned to the players hand immediately
-- after being put into play.
-- Oh great, and reactions can be chosen by the player of which to play, in player order.
-- We need to recursively check for reaction cards after each one is played, too, to update what can be played

-- We need to circle around each player, ask them for which reactions they want to play while updating the possible reactions,
-- and apply the reactions. This won't quite work with what I've done though - you need to ask the player AFTER the answer is
-- available if they wish to play a valid action. So we need to do two phases, one for before reactions and one for after reactions
-- after the event has occurred

redactReactEvent :: Member Obscure r => ReactionEvent Card -> Player -> Sem r (ReactionEvent PotentiallyObscured)
redactReactEvent ev pl = evAnsReaction <$> redactEvent (reactionEvAns ev) pl

-- Prompt the player to react, Maybe signals choosing to not buy
playOneReaction' :: (Member DoReaction r, Member PlayerIO r, Member Obscure r) => Player -> CardEffects (Sem rinitial) a -> Maybe a -> Sem r (Maybe ()) -> Sem r (Maybe ())
playOneReaction' player ceff ma if_invalid = do
  redacted <- redactReactEvent (reactionEvent ceff ma) player
  mreact <- getPlayerReaction player redacted
  case mreact of
    Nothing -> return Nothing
    Just card -> do
     moutcome <- doReaction player card (reactionEvent ceff ma)
     case moutcome of
      Left _   -> if_invalid
      Right outcome -> return $ Just outcome

playOneReaction :: (Member DoReaction r, Member PlayerIO r, Member Obscure r) => Player -> CardEffects (Sem rinnitial) a -> Maybe a -> Sem r (Maybe ())
playOneReaction pl ceff ma = fix $ playOneReaction' pl ceff ma

playerReact :: (Member DoReaction r, Member PlayerIO r, Member Obscure r) => Player -> CardEffects (Sem rinitial) a -> Maybe a -> Sem r [()]
playerReact pl ceff ma = unfoldM (playOneReaction pl ceff ma)

playerReacts :: Members '[DoReaction, CardEffects, PlayerIO, Obscure] r => Player -> CardEffects (Sem rinitial) a -> Sem r a
playerReacts player cardEff = do
  _ <- playerReact player cardEff Nothing -- "before reactions"
  ret <- send (cardEffectrMap cardEff)
  _ <- playerReact player cardEff (Just ret) -- "after reactions"
  return ret

injectReaction :: Members '[BoardStateRead, PlayerIO, CardEffects, Obscure] r => Sem r a -> Sem (DoReaction:r) a
injectReaction program = do
  players' <- getPlayers -- wrong semantics
  let players = Map.keys players' -- probably wrong
  let wah x = Endo $ intercept @CardEffects (playerReacts x)
  appEndo (foldMap wah players) (raise program)

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

interpPlayerIO :: Member DataSerialised r => Sem (PlayerIO : r) a -> Sem r a
interpPlayerIO = interpret (\eff -> dataOut (encode eff) >> untilJust (has @FromJSON eff decode <$> dataIn))

interpPlayerIONoReact :: Member DataSerialised r => Sem (PlayerIO : r) a -> Sem r a
interpPlayerIONoReact = interpret $ \case
  eff@(SendInfo{}) -> dataOut (encode eff)
  eff@(GetPlayerReaction{}) -> return Nothing
  eff -> (dataOut (encode eff) >> untilJust (has @FromJSON eff decode <$> dataIn))

serialiseToTerminal :: Member (Embed IO) r => InterpreterFor DataSerialised r
serialiseToTerminal = interpret $ \case
  DataIn -> embed $ C.fromStrict <$> C.getLine
  DataOut bstr -> embed $ LC.putStrLn bstr

maybePossible :: Members '[DataSerialised] r => PlayerIO (Sem rin) x -> [x] -> Sem r (Maybe x)
maybePossible eff poss = do
  bstr <- dataIn
  case traceShowId $ decode @Int bstr of
    Just n -> return $ poss !? n
    Nothing -> return $ has @FromJSON eff $ decode bstr

interpPlayerIOChoice :: Members '[ValidResponses, DataSerialised] r => InterpreterFor PlayerIO r
interpPlayerIOChoice = interpret $ \eff -> do
  dataOut (encode eff)
  possibilities <- getValidResponses (playerIOmapR eff)
  case possibilities of
    [x] -> return x
    _ -> do
      dataOut . LC.pack $ "Possibilities:"
      has @ToJSON eff $ forM_ (zip [0::Int ..] possibilities) (\(x,y) -> dataOut . mappend (LC.pack . show $ x) . encode $ y)
      untilJust $ maybePossible eff possibilities