module Test where

import Data.List

data CardLang
  -- Basic actions
  = DrawCards Int
  | GainActions Int
  | GainBuys Int
  | GainCoins Int
  
  -- Card movement
  | TrashCard CardFilter FromZone
  | DiscardCard CardFilter FromZone
  | GainCard CardFilter ToZone
  | RevealCard CardFilter FromZone
  | TopdeckCard CardFilter FromZone

  -- Control flow
  | Each Player CardLang         -- apply to each player
  | Optional Player CardLang     -- player may choose to
  | Sequence [CardLang]          -- do these in order
  | Choice Player [CardLang]     -- player chooses one

  -- Conditions
  | When Condition CardLang
  | ForEach CardFilter FromZone CardLang  -- for each matching card, do
  | Repeat Int CardLang    -- do this up to n times, player chooses when to stop

data CardFilter
  = AnyCard
  | OfType CardType
  | OfCost CostFilter
  | InHand
  | Matching [CardFilter]   -- intersection

data CostFilter
  = ExactlyCost Int
  | AtMostCost Int
  | AtLeastCost Int

data CardType
  = Treasure | Victory | Action | Curse

data FromZone = Hand | Deck | Discard | Supply | SetAside
data ToZone   = ToHand | ToDeck | ToDiscard | ToSupply | ToSetAside | ToTrash

data Player = Self | Opponents | AllPlayers | Chooser

data Condition
  = HasCardOfType CardType
  | HandSizeAtLeast Int


-- Cellar is tricky because the draw depends on discard count
-- so we need a slightly richer construct:
cellar :: CardLang
cellar = Sequence
  [ GainActions 1
  , ForEach AnyCard Hand
      (Sequence [ DiscardCard AnyCard Hand
                , DrawCards 1
                ])
  ]

chapel :: CardLang
chapel = Repeat 4 (TrashCard AnyCard Hand)

bandit :: CardLang
bandit = Sequence
  [ GainCard (Matching [OfType Treasure, OfCost (ExactlyCost 6)]) ToDiscard
  , Each Opponents $ Sequence
      [ RevealCard (OfCost (AtLeastCost 0)) Deck
      , When (HasCardOfType Treasure)
          (TrashCard (Matching [OfType Treasure, OfCost (AtLeastCost 2)]) SetAside)
      , DiscardCard AnyCard SetAside
      ]
  ]

describeCardLang :: CardLang -> String
describeCardLang = \case
  DrawCards n ->
    "Draw " <> showCount n "card"

  GainActions n ->
    "+" <> show n <> " Action" <> plural n

  GainBuys n ->
    "+" <> show n <> " Buy" <> plural n

  GainCoins n ->
    "+" <> show n <> " Coin" <> plural n

  TrashCard filter zone ->
    "Trash " <> describeFilter filter
    <> " from your " <> describeFromZone zone

  DiscardCard filter zone ->
    "Discard " <> describeFilter filter
    <> " from your " <> describeFromZone zone

  GainCard filter zone ->
    "Gain " <> describeFilter filter
    <> " to your " <> describeToZone zone

  RevealCard filter zone ->
    "Reveal " <> describeFilter filter
    <> " from your " <> describeFromZone zone

  TopdeckCard filter zone ->
    "Put " <> describeFilter filter
    <> " from your " <> describeFromZone zone <> " onto your deck"

  Each Self inner ->
    describeCardLang inner

  Each Opponents inner ->
    "Each other player " <> describeCardLang inner

  Each AllPlayers inner ->
    "Each player " <> describeCardLang inner

  Each Chooser inner ->
    "The chosen player " <> describeCardLang inner

  Optional player inner ->
    "You may " <> describeCardLang inner

  Choice player options ->
    intercalate " or " (map describeCardLang options)

  Sequence langs ->
    intercalate ". " (map describeCardLang langs)

  When condition inner ->
    "If " <> describeCondition condition
    <> ", " <> describeCardLang inner

  ForEach filter zone inner ->
    "For each " <> describeFilter filter
    <> " in your " <> describeFromZone zone
    <> ", " <> describeCardLang inner

  Repeat n inner ->
    "Up to " <> show n <> " times: " <> describeCardLang inner

describeFilter :: CardFilter -> String
describeFilter = \case
  AnyCard ->
    "a card"
  OfType Treasure ->
    "a Treasure"
  OfType Victory ->
    "a Victory card"
  OfType Action ->
    "an Action card"
  OfType Curse ->
    "a Curse"
  OfCost (ExactlyCost n) ->
    "a card costing exactly " <> show n
  OfCost (AtMostCost n) ->
    "a card costing up to " <> show n
  OfCost (AtLeastCost n) ->
    "a card costing at least " <> show n
  Matching filters ->
    intercalate " and " (map describeFilter filters)
  InHand ->
    "a card in your hand"

describeFromZone :: FromZone -> String
describeFromZone = \case
  Hand     -> "hand"
  Deck     -> "deck"
  Discard  -> "discard pile"
  Supply   -> "supply"
  SetAside -> "set aside area"

describeToZone :: ToZone -> String
describeToZone = \case
  ToHand     -> "hand"
  ToDeck     -> "deck"
  ToDiscard  -> "discard pile"
  ToSupply   -> "supply"
  ToSetAside -> "set aside area"
  ToTrash    -> "trash"

describeCondition :: Condition -> String
describeCondition = \case
  HasCardOfType Treasure -> "you have a Treasure card"
  HasCardOfType Victory  -> "you have a Victory card"
  HasCardOfType Action   -> "you have an Action card"
  HasCardOfType Curse    -> "you have a Curse"
  HandSizeAtLeast n      -> "you have at least " <> show n <> " cards in hand"

plural :: Int -> String
plural 1 = ""
plural _ = "s"

showCount :: Int -> String -> String
showCount n word = show n <> " " <> word <> plural n