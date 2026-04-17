module Interpreters.DoRedact where

import Polysemy
import Types
import Effects

-- This is separate so reactions can obscure cards too when they prompt the player
-- One has to wonder if the structure of these imports implies it could be...yet another effect...
redactEvent :: (Traversable f1, Member Obscure r) => EventAnswer f1 Card -> Player -> Sem r (EventAnswer f1 PotentiallyObscured)
redactEvent = \case 
  a@(EventAnswer (ModifyActions {}) _) ->  logToAll a
  a@(EventAnswer (ModifyBuys {}) _) ->     logToAll a
  a@(EventAnswer (ModifyCurrency {}) _) -> logToAll a
  a@(EventAnswer (ActivateCard _ _) _) ->  logToAll a
  a@(EventAnswer (DrawOnce pl) _) ->       logRedacted pl a
  a@(EventAnswer (BlockOne _ _) _) ->      logToAll a
  a@(EventAnswer (Discard pl _) _) ->      logRedacted pl a
  a@(EventAnswer (TrashCard pl _) _) ->    logRedacted pl a
  a@(EventAnswer (Reveal _ _) _) ->        logToAll a
  a@(EventAnswer (TopDeck pl _) _) ->      logRedacted pl a
  a@(EventAnswer (GainCardTo pl _ _) _) -> logRedacted pl a
  where
    dontRedactCard :: Member Obscure r => Card -> Sem r PotentiallyObscured
    dontRedactCard card = fmap (PObscured . Left . \tid -> (card,tid)) . getTempId $ card

    doRedactCard :: Member Obscure r => Card -> Sem r PotentiallyObscured
    doRedactCard = fmap (PObscured . Right . Obscured) . getTempId

    logToAll :: (Traversable f1, Members '[Obscure] r) => EventAnswer f1 Card -> Player -> Sem r (EventAnswer f1 PotentiallyObscured)
    logToAll eff = const $ traverse dontRedactCard eff

    logRedacted :: (Traversable f1, Members '[Obscure] r) =>
                   Player ->
                   EventAnswer f1 Card ->
                   Player ->
                   Sem r (EventAnswer f1 PotentiallyObscured)
    logRedacted pl eff pl2 = do
      secret <- traverse dontRedactCard eff
      public <- traverse doRedactCard eff
      return $ if pl == pl2 then secret else public