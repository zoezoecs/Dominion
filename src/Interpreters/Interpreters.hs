{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Interpreters.Interpreters (
  logEffects,
  logTurn,
  logPlayerToString,
  logPlayerToPlayerIO,
  logToPlayerLog,
  injectReaction,
  interpStacks,
  interpDoReaction,
  interpRandomWithSeed,
  interpRandomGlobal,
  interpRandomShuffle,
  interpCardEffects,
  runCorrelation,
  interpGameLoop,
  interpGameRules,
  interpPlayerIO,
  interpPlayerIONoReact,
  interpStateRead,
  runRandomUniqueId,
  serialiseToTerminal,
) where

import Interpreters.Log (logEffects, logTurn, logPlayerToString, logToPlayerLog, runCorrelation, logPlayerToPlayerIO)
import Interpreters.Stacks (interpStacks)
import Interpreters.Random (interpRandomWithSeed, interpRandomGlobal, interpRandomShuffle, runRandomUniqueId)
import Interpreters.Other (interpCardEffects, interpPlayerIO, interpStateRead, injectReaction, serialiseToTerminal, interpPlayerIONoReact)
import Interpreters.GameLogic (interpGameLoop, interpGameRules, interpDoReaction)