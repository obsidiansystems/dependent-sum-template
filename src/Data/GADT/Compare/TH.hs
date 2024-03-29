{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
module Data.GADT.Compare.TH
    ( DeriveGEQ(..)
    , DeriveGCompare(..)
    , module Data.GADT.Compare.Monad
    ) where

import Control.Monad
import Control.Monad.Writer
import Data.GADT.TH.Internal
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.Monad
import Data.Type.Equality ((:~:) (..))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Map (Map)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- A type class purely for overloading purposes
class DeriveGEQ t where
    deriveGEq :: t -> Q [Dec]

instance DeriveGEQ Name where
  deriveGEq typeName = do
    typeInfo <- reifyDatatype typeName
    let instTypes = datatypeInstTypes typeInfo
        paramVars = Set.unions [freeTypeVariables t | t <- instTypes]
        instTypes' = case reverse instTypes of
          [] -> fail "deriveGEq: Not enough type parameters"
          (_:xs) -> reverse xs
        instanceHead = AppT (ConT ''GEq) (foldl AppT (ConT typeName) instTypes')
    (clauses, cxt) <- runWriterT (mapM (geqClause paramVars) (datatypeCons typeInfo))

    return [InstanceD Nothing cxt instanceHead [geqFunction clauses]]

instance DeriveGEQ Dec where
    deriveGEq = deriveForDec ''GEq $ \typeInfo -> do
      let
        instTypes = datatypeInstTypes typeInfo
        paramVars = Set.unions [freeTypeVariables t | t <- instTypes]
      clauses <- mapM (geqClause paramVars) (datatypeCons typeInfo)
      return $ geqFunction clauses

instance DeriveGEQ t => DeriveGEQ [t] where
  deriveGEq [it] = deriveGEq it
  deriveGEq _ = fail "deriveGEq: [] instance only applies to single-element lists"

instance DeriveGEQ t => DeriveGEQ (Q t) where
  deriveGEq = (>>= deriveGEq)

geqFunction :: [Clause] -> Dec
geqFunction clauses = FunD 'geq $ clauses ++ [ Clause [WildP, WildP] (NormalB (ConE 'Nothing)) [] ]
 -- TODO: only include last clause if there's more than one constructor?

geqClause :: Set Name -> ConstructorInfo -> WriterT Cxt Q Clause
geqClause paramVars con = do
  let conName = constructorName con
      argTypes = constructorFields con
      conTyVars = Set.fromList (map tvName (constructorVars con))
      needsGEq argType = not . Set.null $
        Set.intersection (freeTypeVariables argType) (Set.union paramVars conTyVars)
  lArgNames <- forM argTypes $ \_ -> lift $ newName "x"
  rArgNames <- forM argTypes $ \_ -> lift $ newName "y"

  stmts <- forM (zip3 lArgNames rArgNames argTypes) $ \(l, r, t) -> do
    case t of
      AppT tyFun tyArg | needsGEq t -> do
        u <- lift $ reifyInstancesWithRigids paramVars ''GEq [tyFun]
        case u of
          [] -> tell [AppT (ConT ''GEq) tyFun]
          [(InstanceD _ cxt _ _)] -> tell cxt
          _ -> fail $ "More than one instance found for GEq (" ++ show (ppr tyFun) ++ "), and unsure what to do. Please report this."
        lift $ bindS (conP 'Refl []) [| geq $(varE l) $(varE r) |]
      _ -> lift $ noBindS [| guard ($(varE l) == $(varE r)) |]
  ret <- lift $ noBindS [| return Refl |]

  pats <- lift $ sequence
    [ conP conName (map varP lArgNames)
    , conP conName (map varP rArgNames)
    ]
  pure $ Clause pats
    (NormalB (doUnqualifiedE (stmts ++ [ret])))
    []

class DeriveGCompare t where
    deriveGCompare :: t -> Q [Dec]

instance DeriveGCompare Name where
    deriveGCompare typeName = do
      typeInfo <- reifyDatatype typeName
      let instTypes = datatypeInstTypes typeInfo
          paramVars = Set.unions [freeTypeVariables t | t <- instTypes]
          instTypes' = case reverse instTypes of
            [] -> fail "deriveGCompare: Not enough type parameters"
            (_:xs) -> reverse xs
          instanceHead = AppT (ConT ''GCompare) (foldl AppT (ConT typeName) instTypes')
      (clauses, cxt) <- runWriterT (fmap concat $ mapM (gcompareClauses paramVars) (datatypeCons typeInfo))
      dec <- gcompareFunction clauses
      return [InstanceD Nothing cxt instanceHead [dec]]

instance DeriveGCompare Dec where
    deriveGCompare = deriveForDec ''GCompare $ \typeInfo -> do
      let
        instTypes = datatypeInstTypes typeInfo
        paramVars = Set.unions [freeTypeVariables t | t <- instTypes]
      clauses <- mapM (gcompareClauses paramVars) (datatypeCons typeInfo)
      lift $ gcompareFunction (concat clauses)

instance DeriveGCompare t => DeriveGCompare [t] where
    deriveGCompare [it] = deriveGCompare it
    deriveGCompare _ = fail "deriveGCompare: [] instance only applies to single-element lists"

instance DeriveGCompare t => DeriveGCompare (Q t) where
    deriveGCompare = (>>= deriveGCompare)

gcompareFunction :: [Clause] -> Q Dec
gcompareFunction [] = funD 'gcompare [clause [] (normalB [| \x y -> seq x (seq y undefined) |]) []]
gcompareFunction clauses = return $ FunD 'gcompare clauses

gcompareClauses :: Set Name -> ConstructorInfo -> WriterT Cxt Q [Clause]
gcompareClauses paramVars con = do
  let conName = constructorName con
      argTypes = constructorFields con
      conTyVars = Set.fromList (map tvName (constructorVars con))
      needsGCompare argType = not . Set.null $
        Set.intersection (freeTypeVariables argType) (Set.union paramVars conTyVars)

  lArgNames <- forM argTypes $ \_ -> lift $ newName "x"
  rArgNames <- forM argTypes $ \_ -> lift $ newName "y"

  stmts <- forM (zip3 lArgNames rArgNames argTypes) $ \(lArg, rArg, argType) ->
    case argType of
      AppT tyFun tyArg | needsGCompare argType -> do
        u <- lift $ reifyInstancesWithRigids paramVars ''GCompare [tyFun]
        case u of
          [] -> tell [AppT (ConT ''GCompare) tyFun]
          [(InstanceD _ cxt _ _)] -> tell cxt -- this might not be enough, may want to do full instance resolution.
          _ -> fail $ "More than one instance of GCompare (" ++ show (ppr tyFun) ++ ") found, and unsure what to do. Please report this."
        lift $ bindS (conP 'Refl []) [| geq' $(varE lArg) $(varE rArg) |]
      _ -> lift $ noBindS [| compare' $(varE lArg) $(varE rArg) |]

  ret <- lift $ noBindS [| return GEQ |]


  pats <- lift $ sequence
        [ conP conName (map varP lArgNames)
        , conP conName (map varP rArgNames)
        ]
  let main = Clause pats
        (NormalB (AppE (VarE 'runGComparing) (doUnqualifiedE (stmts ++ [ret]))))
        []
      lt = Clause [RecP conName [], WildP] (NormalB (ConE 'GLT)) []
      gt = Clause [WildP, RecP conName []] (NormalB (ConE 'GGT)) []
  return [main, lt, gt]

#if MIN_VERSION_template_haskell(2,17,0)
doUnqualifiedE = DoE Nothing
#else
doUnqualifiedE = DoE
#endif
