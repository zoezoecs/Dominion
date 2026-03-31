{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Interpreters.Interpreters (
  logEffects,
  logTurn,
  logPlayerToString,
  logToPlayerLog,
  interpStacks,
  interpRandomWithSeed,
  interpRandomGlobal,
  interpRandomShuffle,
  interpCardEffects,
  interpCorrelation,
  interpGameLoop,
  interpGameRules,
  interpPlayerIO,
  interpStateRead,
) where

import Interpreters.Log (logEffects, logTurn, logPlayerToString, logToPlayerLog)
import Interpreters.Stacks (interpStacks)
import Interpreters.Random (interpRandomWithSeed, interpRandomGlobal, interpRandomShuffle)
import Interpreters.Other (interpCardEffects, interpCorrelation, interpGameLoop, interpGameRules, interpPlayerIO, interpStateRead)