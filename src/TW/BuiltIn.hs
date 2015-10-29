{-# LANGUAGE OverloadedStrings #-}
module TW.BuiltIn
    ( BuiltIn(..)
    , allBuiltIns
    , tyString, tyInt, tyFloat, tyBool, tyMaybe
    )
where

import TW.Ast

import qualified Data.Text as T

data BuiltIn
   = BuiltIn
   { bi_name :: QualTypeName
   , bi_args :: [TypeVar]
   } deriving (Show, Eq)

allBuiltIns :: [BuiltIn]
allBuiltIns = [tyString, tyInt, tyFloat, tyBool, tyMaybe]

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
