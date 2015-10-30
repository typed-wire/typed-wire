{-# LANGUAGE OverloadedStrings #-}
module TW.BuiltIn
    ( BuiltIn(..)
    , allBuiltIns, isBuiltIn
    , tyString, tyInt, tyFloat, tyBool, tyMaybe, tyBytes, tyList
    )
where

import TW.Ast

import qualified Data.List as L
import qualified Data.Text as T

data BuiltIn
   = BuiltIn
   { bi_name :: QualTypeName
   , bi_args :: [TypeVar]
   } deriving (Show, Eq)

allBuiltIns :: [BuiltIn]
allBuiltIns = [tyString, tyInt, tyFloat, tyBool, tyMaybe, tyBytes, tyList]

isBuiltIn :: Type -> Maybe (BuiltIn, [Type])
isBuiltIn ty =
    case ty of
      TyVar _ -> Nothing
      TyCon qt args ->
          let r = flip L.find allBuiltIns $ \bi -> bi_name bi == qt && length (bi_args bi) == length args
          in case r of
               Just x -> Just (x, args)
               _ -> Nothing

builtIn :: T.Text -> BuiltIn
builtIn = flip builtInVars []

builtInVars :: T.Text -> [T.Text] -> BuiltIn
builtInVars x v = BuiltIn (QualTypeName (ModuleName []) (TypeName x)) (map TypeVar v)

tyString :: BuiltIn
tyString = builtIn "String"

tyInt :: BuiltIn
tyInt = builtIn "Int"

tyBool :: BuiltIn
tyBool = builtIn "Bool"

tyFloat :: BuiltIn
tyFloat = builtIn "Float"

tyMaybe :: BuiltIn
tyMaybe = builtInVars "Maybe" ["a"]

tyList :: BuiltIn
tyList = builtInVars "List" ["a"]

tyBytes :: BuiltIn
tyBytes = builtIn "Bytes"
