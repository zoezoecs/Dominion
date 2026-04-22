{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

{-# OPTIONS_HADDOCK not-home #-}

-- Mostly copied from Polysemy.Internal.TH

module Internal.TH
  ( makeSemMonomorphised, genNoR, genNoR'
  ) where

import Control.Monad
import Language.Haskell.TH

import Language.Haskell.TH.Syntax (addModFinalizer)

import Language.Haskell.TH.Datatype
import Polysemy.Internal.TH.Common
import Debug.Trace

import Data.List
import qualified Data.Map as Map

-- NOTE(makeSem_):
-- This function uses an ugly hack to work --- it changes names in data
-- constructor's type to capturable ones. This allows users to provide them to
-- us from their signature through 'forall' with 'ScopedTypeVariables'
-- enabled, so that we can compile liftings of constructors with ambiguous
-- type arguments (see issue #48).
--
-- Please, change this as soon as GHC provides some way of inspecting
-- signatures, replacing code or generating haddock documentation in TH.

makeSemMonomorphised :: Name -> Name -> Q [Dec]
makeSemMonomorphised monoName effName = do
  genFreerMonomorphised (ConT monoName) True effName

replaceType :: Type -> Type -> Type -> Type
replaceType search_this replace_this = go
  where
    go1 :: Type -> Type
    go1 x = if x == search_this then replace_this else go x

    go :: Type -> Type
    go (AppT t1 t2) = AppT (go1 t1) (go1 t2)
    go (AppKindT k t1) = AppKindT k (go1 t1)
    go (SigT t1 k) = SigT (go1 t1) k
    go (InfixT t1 n t2) = InfixT (go1 t1) n (go1 t2)
    go (UInfixT t1 n t2) = UInfixT (go1 t1) n (go1 t2)
    go (PromotedInfixT t1 n t2) = PromotedInfixT (go1 t1) n (go1 t2)
    go (PromotedUInfixT t1 n t2) = PromotedUInfixT (go1 t1) n (go1 t2)
    go (ParensT t1) = ParensT t1
    go x = x

monomorphiseConLiftInfo :: Type -> ConLiftInfo -> ConLiftInfo
monomorphiseConLiftInfo new_type di =
  let
    [prev_type] = cliEffArgs di
    effRes = cliEffRes di
    funArgs = cliFunArgs di
    funCxt = cliFunCxt di
    replaceFun (name, ty) = if ty==prev_type then (name, new_type) else (name, replaceType prev_type new_type ty)
  in
    di{
      cliEffArgs=[new_type],
      cliEffRes=replaceType prev_type new_type effRes,
      cliFunArgs=map replaceFun funArgs,
      cliFunCxt=map (replaceType prev_type new_type) funCxt
      }

genFreerMonomorphised :: Type -> Bool -> Name -> Q [Dec]
genFreerMonomorphised mono should_mk_sigs type_name = do
  checkExtensions [ScopedTypeVariables, FlexibleContexts, DataKinds]
  cl_infos' <- getEffectMetadata type_name
  let cl_infos = fmap (monomorphiseConLiftInfo mono) cl_infos'
  decs <- traverse (genDec should_mk_sigs) cl_infos

  let sigs = if should_mk_sigs then genSig <$> cl_infos else []
  pure $ join $ sigs ++ decs

-- Generates a signature for a function Effect m1 a -> Effect m2 a.
-- chR_Eff :: Eff m1 a -> Eff m2 a
-- chR_Eff (Con1 arg) = Con1 arg
-- chR_Eff (Con2 arg arg) = Con2 arg arg
genNoR :: Name -> Q [Dec]
genNoR = genNoR' mempty

-- None of these approaches seem to work because of kind unification issues
-- data FOCoerce f = forall {k1} {k2} (m1 :: k1) (m2 :: k2) (a :: *). Gah (f m1 a -> f m2 a)
-- 
-- class FO eff where
--   chR :: forall {k1} {k2} (m1 :: k1) (m2 :: k2) a. eff m1 a -> eff m2 a

-- instance FO (CardEffects' card) where
--   chR = chR_CardEffects'

-- Generates a signature for a function Effect m1 a -> Effect m2 a. Uses Map to replace variables with the key type
-- with the value type applied to the variable, to allow for ad hoc coercions of other effects that occur in the constructor
-- chR_Eff :: Eff m1 a -> Eff m2 a
-- chR_Eff (Con1 arg) = Con1 arg
-- chR_Eff (Con2 arg arg) = Con2 arg (chR_Eff2 arg)

genNoR' :: Map.Map Name Name -> Name -> Q [Dec]
genNoR' coerce_map type_name = do
  let should_mk_sigs = True
  checkExtensions [ScopedTypeVariables, FlexibleContexts, DataKinds]
  cl_infos <- getEffectMetadata type_name
  let fn_name = mkName $ "chR_" ++ nameBase type_name
  let coerce_map_type = Map.mapKeys ConT coerce_map
  decs <- genMyDec coerce_map_type fn_name cl_infos

  let sigs = if should_mk_sigs then genMySig (cl_infos!!0) fn_name else []
  pure $ sigs ++ decs

------------------------------------------------------------------------------
-- | Generates signature for lifting function and type arguments to apply in
-- its body on effect's data constructor.
genSig :: ConLiftInfo -> [Dec]
genSig cli =
  infixDecl
  ++ [ SigD (cliFunName cli) $ quantifyType
       $ ForallT [] (member_cxt : cliFunCxt cli)
       $ foldArrowTs sem
       $ fmap snd
       $ cliFunArgs cli
     ]
  where
    infixDecl = case cliFunFixity cli of



      Just fixity -> [InfixD fixity (cliFunName cli)]

      Nothing -> []
    member_cxt = makeMemberConstraint (cliUnionName cli) cli
    sem        = makeSemType (cliUnionName cli) (cliEffRes cli)


------------------------------------------------------------------------------
-- | Builds a function definition of the form
-- @x a b c = send (X a b c :: E m a)@.
genDec :: Bool -> ConLiftInfo -> Q [Dec]
genDec should_mk_sigs cli = do
  let fun_args_names = fst <$> cliFunArgs cli

  doc <- getDoc $ DeclDoc $ cliConName cli
  maybe (pure ()) (addModFinalizer . putDoc (DeclDoc $ cliFunName cli)) doc

  pure
    [ PragmaD $ InlineP (cliFunName cli) Inlinable ConLike AllPhases
    , FunD (cliFunName cli)
        [ Clause (VarP <$> fun_args_names)
                 (NormalB $ makeUnambiguousSend should_mk_sigs cli)
                 []
        ]
    ]

getHead :: Type -> Type
getHead (AppT x _) = getHead x
getHead x = x

makeArg :: Map.Map Type Name -> (Name, Type) -> Exp
makeArg mapper (n, t) = case Map.lookup (getHead t) mapper of
  Nothing -> VarE n
  Just fn -> AppE (VarE fn) (VarE n)

-- Constructs (Constructor arg1 arg2 arg3)
makeAppliedConstructor :: Map.Map Type Name -> ConLiftInfo  -> Exp
makeAppliedConstructor mapper cli =
 foldl1' AppE $ ConE (cliConName cli) : (makeArg mapper <$> cliFunArgs cli)

genMyDec :: Map.Map Type Name -> Name -> [ConLiftInfo] -> Q [Dec]
genMyDec coerce_map fn_name clis = do
    let fun_args_namess = fmap (\x -> (x, makeAppliedConstructor coerce_map x)) clis
    pure
      [
        FunD fn_name
          [ Clause [ConP (cliConName concon) [] (VarP <$> (fst <$> cliFunArgs concon))]
                   (NormalB x)
                   [] | (concon, x) <- fun_args_namess
          ]
      ]

genMySig :: ConLiftInfo -> Name -> [Dec]
genMySig cli fn_name =
  infixDecl
  ++ [ SigD fn_name
       $ AppT (AppT (makeEffectType cli) (VarT (mkName "m1"))) (VarT (mkName "a")) :-> AppT (AppT (makeEffectType cli) (VarT (mkName "m2"))) (VarT (mkName "a"))
     ]
  where
    infixDecl = case cliFunFixity cli of



      Just fixity -> [InfixD fixity (cliFunName cli)]

      Nothing -> []