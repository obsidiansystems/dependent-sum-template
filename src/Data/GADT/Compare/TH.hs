{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PolyKinds #-}
#endif
module Data.GADT.Compare.TH
    ( DeriveGEQ(..)
    , DeriveGCompare(..)
    , DeriveEqTagIdentity(..)
    , DeriveOrdTagIdentity(..)
    , GComparing, runGComparing, geq', compare'
    ) where

import Control.Applicative
import Control.Monad
import Data.Dependent.Sum
import Data.Dependent.Sum.TH.Internal
import Data.Functor.Identity
import Data.GADT.Compare
import Language.Haskell.TH
import Language.Haskell.TH.Extras

-- A type class purely for overloading purposes
class DeriveGEQ t where
    deriveGEq :: t -> Q [Dec]

instance DeriveGEQ Name where
    deriveGEq typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGEq dec
            _ -> fail "deriveGEq: the name of a type constructor is required"

instance DeriveGEQ Dec where
    deriveGEq = deriveForDec ''GEq (\t -> [t| GEq $t |]) geqFunction

instance DeriveGEQ t => DeriveGEQ [t] where
    deriveGEq [it] = deriveGEq it
    deriveGEq _ = fail "deriveGEq: [] instance only applies to single-element lists"

instance DeriveGEQ t => DeriveGEQ (Q t) where
    deriveGEq = (>>= deriveGEq)

geqFunction bndrs cons = funD 'geq
    (  map (geqClause bndrs) cons
    ++  [ clause [wildP, wildP] (normalB [| Nothing |]) []
        | length cons /= 1
        ]
    )

geqClause bndrs con = do
    let argTypes = argTypesOfCon con
        needsGEq argType = any ((`occursInType` argType) . nameOfBinder) (bndrs ++ varsBoundInCon con)

        nArgs = length argTypes
    lArgNames <- replicateM nArgs (newName "x")
    rArgNames <- replicateM nArgs (newName "y")

    clause [ conP conName (map varP lArgNames)
           , conP conName (map varP rArgNames)
           ]
        ( normalB $ doE
            (  [ if needsGEq argType
                    then bindS (conP 'Refl []) [| geq $(varE lArg) $(varE rArg) |]
                    else noBindS [| guard ($(varE lArg) == $(varE rArg)) |]
               | (lArg, rArg, argType) <- zip3 lArgNames rArgNames argTypes
               ]
            ++ [ noBindS [| return Refl |] ]
            )
        ) []
    where conName = nameOfCon con

-- A monad allowing gcompare to be defined in the same style as geq
newtype GComparing a b t = GComparing (Either (GOrdering a b) t)

instance Functor (GComparing a b) where fmap f (GComparing x) = GComparing (either Left (Right . f) x)
instance Monad (GComparing a b) where
    return = GComparing . Right
    GComparing (Left  x) >>= f = GComparing (Left x)
    GComparing (Right x) >>= f = f x
instance Applicative (GComparing a b) where
    pure = return
    (<*>) = ap

geq' :: GCompare t => t a -> t b -> GComparing x y (a := b)
geq' x y = GComparing (case gcompare x y of
    GLT -> Left GLT
    GEQ -> Right Refl
    GGT -> Left GGT)

compare' x y = GComparing $ case compare x y of
    LT -> Left GLT
    EQ -> Right ()
    GT -> Left GGT

runGComparing (GComparing x) = either id id x

class DeriveGCompare t where
    deriveGCompare :: t -> Q [Dec]

instance DeriveGCompare Name where
    deriveGCompare typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGCompare dec
            _ -> fail "deriveGCompare: the name of a type constructor is required"

instance DeriveGCompare Dec where
    deriveGCompare = deriveForDec ''GCompare (\t -> [t| GCompare $t |]) gcompareFunction

instance DeriveGCompare t => DeriveGCompare [t] where
    deriveGCompare [it] = deriveGCompare it
    deriveGCompare _ = fail "deriveGCompare: [] instance only applies to single-element lists"

instance DeriveGCompare t => DeriveGCompare (Q t) where
    deriveGCompare = (>>= deriveGCompare)

gcompareFunction boundVars cons
    | null cons = funD 'gcompare [clause [] (normalB [| \x y -> seq x (seq y undefined) |]) []]
    | otherwise = funD 'gcompare (concatMap gcompareClauses cons)
    where
        -- for every constructor, first check for equality (recursively comparing
        -- arguments) then add catch-all cases; all not-yet-matched patterns are
        -- "greater than" the constructor under consideration.
        gcompareClauses con =
            [ mainClause con
            , clause [recP conName [], wildP] (normalB [| GLT |]) []
            , clause [wildP, recP conName []] (normalB [| GGT |]) []
            ] where conName = nameOfCon con

        needsGCompare argType con = any ((`occursInType` argType) . nameOfBinder) (boundVars ++ varsBoundInCon con)

        -- main clause; using the 'GComparing' monad, compare all arguments to the
        -- constructor recursively, attempting to unify type variables by recursive
        -- calls to gcompare whenever needed (that is, whenever a constructor argument's
        -- type contains a variable bound in the data declaration or in the constructor's
        -- type signature)
        mainClause con = do
            let conName = nameOfCon con
                argTypes = argTypesOfCon con
                nArgs = length argTypes

            lArgNames <- replicateM nArgs (newName "x")
            rArgNames <- replicateM nArgs (newName "y")

            clause [ conP conName (map varP lArgNames)
                   , conP conName (map varP rArgNames)
                   ]
                ( normalB
                    [| runGComparing $
                        $(doE
                            (  [ if needsGCompare argType con
                                    then bindS (conP 'Refl []) [| geq' $(varE lArg) $(varE rArg) |]
                                    else noBindS [| compare' $(varE lArg) $(varE rArg) |]
                               | (lArg, rArg, argType) <- zip3 lArgNames rArgNames argTypes
                               ]
                            ++ [ noBindS [| return GEQ |] ]
                            )
                        )
                    |]
                ) []

-- A type class purely for overloading purposes
class DeriveEqTagIdentity t where
    deriveEqTagIdentity :: t -> Q [Dec]

instance DeriveEqTagIdentity Name where
    deriveEqTagIdentity typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveEqTagIdentity dec
            _ -> fail "deriveEqTagIdentity: the name of a type constructor is required"

instance DeriveEqTagIdentity Dec where
    deriveEqTagIdentity = deriveForDec ''EqTag (\t -> [t| EqTag $t Identity |]) eqTaggedFunction

instance DeriveEqTagIdentity t => DeriveEqTagIdentity [t] where
    deriveEqTagIdentity [it] = deriveEqTagIdentity it
    deriveEqTagIdentity _ = fail "deriveEqTagIdentity: [] instance only applies to single-element lists"

instance DeriveEqTagIdentity t => DeriveEqTagIdentity (Q t) where
    deriveEqTagIdentity = (>>= deriveEqTagIdentity)

eqTaggedFunction bndrs cons = funD 'eqTagged $
    map (eqTaggedClause bndrs) cons

eqTaggedClause bndrs con = do
    let argTypes = argTypesOfCon con

    clause [ conP conName (wildP <$ argTypes)
           , wildP
           ]
        ( normalB [| (==) |]
        ) []
    where conName = nameOfCon con

-- A type class purely for overloading purposes
class DeriveOrdTagIdentity t where
    deriveOrdTagIdentity :: t -> Q [Dec]

instance DeriveOrdTagIdentity Name where
    deriveOrdTagIdentity typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveOrdTagIdentity dec
            _ -> fail "deriveOrdTagIdentity: the name of a type constructor is required"

instance DeriveOrdTagIdentity Dec where
    deriveOrdTagIdentity = deriveForDec ''OrdTag (\t -> [t| OrdTag $t Identity |]) compareTaggedFunction

instance DeriveOrdTagIdentity t => DeriveOrdTagIdentity [t] where
    deriveOrdTagIdentity [it] = deriveOrdTagIdentity it
    deriveOrdTagIdentity _ = fail "deriveOrdTagIdentity: [] instance only applies to single-element lists"

instance DeriveOrdTagIdentity t => DeriveOrdTagIdentity (Q t) where
    deriveOrdTagIdentity = (>>= deriveOrdTagIdentity)

compareTaggedFunction bndrs cons = funD 'compareTagged $
    map (compareTaggedClause bndrs) cons

compareTaggedClause bndrs con = do
    let argTypes = argTypesOfCon con

    clause [ conP conName (wildP <$ argTypes)
           , wildP
           ]
        ( normalB [| compare |]
        ) []
    where conName = nameOfCon con
