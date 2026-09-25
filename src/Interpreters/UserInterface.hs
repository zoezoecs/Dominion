module Interpreters.UserInterface where

import Polysemy
import Polysemy.State

import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import Data.Constraint.Extras
--import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Text.Read (readMaybe)


import Base
import Types
import Effects

-- A line-based debug command, typed instead of a numeric choice.
data DebugCommand
  = CmdHand Int
  | CmdDeck Int
  | CmdDiscard Int
  | CmdVP Int
  | CmdActive
  | CmdGameState
  | CmdHelp

parseDebugCommand :: LC.ByteString -> Maybe DebugCommand
parseDebugCommand line = case words (LC.unpack line) of
  (":hand"    : n : _) -> CmdHand    <$> readMaybe n
  (":deck"    : n : _) -> CmdDeck    <$> readMaybe n
  (":discard" : n : _) -> CmdDiscard <$> readMaybe n
  (":vp"      : n : _) -> CmdVP      <$> readMaybe n
  (":active"  :   _  ) -> Just CmdActive
  (":state"   :   _  ) -> Just CmdGameState
  (":help"    :   _  ) -> Just CmdHelp
  _                    -> Nothing

runDebugCommand :: Members '[BoardStateRead, Stacks, State GameState, DataSerialised] r => DebugCommand -> Sem r ()
runDebugCommand (CmdHand n)    = getHand (MkPlayer n)        >>= dataOut . LC.pack . show
runDebugCommand (CmdDeck n)    = getDeck (MkPlayer n)        >>= dataOut . LC.pack . show
runDebugCommand (CmdDiscard n) = getDiscardPile (MkPlayer n) >>= dataOut . LC.pack . show
runDebugCommand (CmdVP n)      = getVP (MkPlayer n)          >>= dataOut . LC.pack . show
runDebugCommand CmdActive      = activeSupplies              >>= dataOut . LC.pack . show
runDebugCommand CmdGameState   = get @GameState               >>= dataOut . LC.pack . show
runDebugCommand CmdHelp        = dataOut . LC.pack $ unlines
  [ ":hand N     - show player N's hand"
  , ":deck N     - show player N's deck"
  , ":discard N  - show player N's discard pile"
  , ":vp N       - show player N's victory points"
  , ":active     - show active (non-empty) supply piles"
  , ":state      - show full GameState"
  , ":help       - show this message"
  ]

maybePossible
  :: Members '[BoardStateRead, Stacks, State GameState, DataSerialised] r
  => PlayerIO (Sem rin) x -> [x] -> Sem r (Maybe x)
maybePossible eff poss = do
  bstr <- dataIn
  case parseDebugCommand bstr of
    Just cmd -> runDebugCommand cmd >> pure Nothing  -- Nothing => untilJust re-prompts
    Nothing  -> case decode @Int bstr of
      Just n -> case poss !? n of
        Just x  -> pure $ Just x
        Nothing -> dataOut (LC.pack "No such option, try again:") >> pure Nothing
      Nothing -> case has @FromJSON eff (decode bstr) of
        Just x  -> pure $ Just x
        Nothing -> dataOut (LC.pack "Couldn't parse that (try a number, or :help):") >> pure Nothing


interpPlayerIOChoice :: Members '[ValidResponses, BoardStateRead, Stacks, State GameState, DataSerialised] r => InterpreterFor PlayerIO r
interpPlayerIOChoice = interpret $ \eff -> do
  dataOut . LC.pack $ show eff
  possibilities <- getValidResponses (playerIOmapR eff)
  case possibilities of
    [x] -> pure x
    _   -> do
      case getAddressedPlayer eff of
        Just (MkPlayer pl) -> dataOut . LC.pack $ "\nPLAYER TURN:" <> show pl
        Nothing -> pure ()
      dataOut . LC.pack $ "Possibilities:"
      has @Show eff $ forM_ (zip [0::Int ..] possibilities) $ \(i, y) ->
        dataOut . LC.pack $ show i ++ ": " ++ show y
      dataOut . LC.pack $ "Enter a number (or :help for inspection commands):"
      res <- untilJust $ maybePossible eff possibilities
      dataOut . LC.pack $ "\n"
      pure res



interpPlayerIO :: Member DataSerialised r => Sem (PlayerIO : r) a -> Sem r a
interpPlayerIO = interpret (\eff -> dataOut (encode eff) >> untilJust (has @FromJSON eff decode <$> dataIn))

interpPlayerIONoReact :: Member DataSerialised r => Sem (PlayerIO : r) a -> Sem r a
interpPlayerIONoReact = interpret $ \case
  eff@(SendInfo{}) -> dataOut (encode eff)
  (GetPlayerReaction{}) -> pure Nothing
  eff -> (dataOut (encode eff) >> untilJust (has @FromJSON eff decode <$> dataIn))

serialiseToTerminal :: Member (Embed IO) r => InterpreterFor DataSerialised r
serialiseToTerminal = interpret $ \case
  DataIn -> embed $ C.fromStrict <$> C.getLine
  DataOut bstr -> embed $ LC.putStrLn bstr