{-# LANGUAGE CPP, TemplateHaskell #-}
module Data.GADT.Show.TH
    ( DeriveGShow(..)
    , DeriveShowTagIdentity(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Dependent.Sum
import Data.Dependent.Sum.TH.Internal
import Data.Functor.Identity
import Data.GADT.Show
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Extras

class DeriveGShow t where
    deriveGShow :: t -> Q [Dec]

instance DeriveGShow Name where
    deriveGShow typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGShow dec
            _ -> fail "deriveGShow: the name of a type constructor is required"

instance DeriveGShow Dec where
    deriveGShow = deriveForDec ''GShow (\t -> [t| GShow $t |]) gshowFunction

instance DeriveGShow t => DeriveGShow [t] where
    deriveGShow [it] = deriveGShow it
    deriveGShow _ = fail "deriveGShow: [] instance only applies to single-element lists"

instance DeriveGShow t => DeriveGShow (Q t) where
    deriveGShow = (>>= deriveGShow)

gshowFunction bndrs cons = funD 'gshowsPrec $ gshowClause bndrs <$> cons

gshowClause bndrs con = do
    let conName  = nameOfCon con
        argTypes = argTypesOfCon con
        needsGShow argType = any ((`occursInType` argType) . nameOfBinder) (bndrs ++ varsBoundInCon con)

        nArgs    = length argTypes

        precName = mkName "p"

    argNames <- replicateM nArgs (newName "x")

    let precPat = if null argNames
          then wildP
          else varP precName

        showsName name = [| showString $(litE . stringL $ nameBase name) |]

        gshowBody = composeExprs $ intersperse [| showChar ' ' |]
            $ showsName conName
            : [ if needsGShow argType
                   then [| gshowsPrec 11 $arg |]
                   else [| showsPrec 11 $arg |]
              | (argName, argType) <- zip argNames argTypes
              , let arg = varE argName
              ]

        gshowBody' = case argNames of
          [] -> gshowBody
          _ -> [| showParen ($(varE precName) > 10) $gshowBody |]

    clause [precPat, conP conName (map varP argNames)] (normalB gshowBody') []

-- A type class purely for overloading purposes
class DeriveShowTagIdentity t where
    deriveShowTagIdentity :: t -> Q [Dec]

instance DeriveShowTagIdentity Name where
    deriveShowTagIdentity typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveShowTagIdentity dec
            _ -> fail "deriveShowTagIdentity: the name of a type constructor is required"

instance DeriveShowTagIdentity Dec where
    deriveShowTagIdentity = deriveForDec ''ShowTag (\t -> [t| ShowTag $t Identity |]) showTaggedFunction

instance DeriveShowTagIdentity t => DeriveShowTagIdentity [t] where
    deriveShowTagIdentity [it] = deriveShowTagIdentity it
    deriveShowTagIdentity _ = fail "deriveShowTagIdentity: [] instance only applies to single-element lists"

instance DeriveShowTagIdentity t => DeriveShowTagIdentity (Q t) where
    deriveShowTagIdentity = (>>= deriveShowTagIdentity)

showTaggedFunction bndrs cons = funD 'showTaggedPrec $
    map (showTaggedClause bndrs) cons

showTaggedClause bndrs con = do
    let argTypes = argTypesOfCon con

    clause [ conP conName (wildP <$ argTypes)
           ]
        ( normalB [| showsPrec |]
        ) []
    where conName = nameOfCon con
