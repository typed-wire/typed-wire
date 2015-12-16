{-# LANGUAGE OverloadedStrings #-}
module TW.BuiltIn
    ( BuiltIn(..)
    , allBuiltIns, isBuiltIn, builtInAsType, pathPieceTypes, isPathPiece
    , tyString, tyInt, tyFloat, tyBool, tyMaybe, tyBytes, tyList, tyDateTime, tyDate, tyTime
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

builtInAsType :: BuiltIn -> Type
builtInAsType (BuiltIn qt args) = TyCon qt (map TyVar args)

allBuiltIns :: [BuiltIn]
allBuiltIns = [tyString, tyInt, tyFloat, tyBool, tyMaybe, tyBytes, tyList, tyDateTime, tyDate, tyTime]

pathPieceTypes :: [BuiltIn]
pathPieceTypes = [tyString, tyInt, tyFloat, tyBool]

isPathPiece :: Type -> Bool
isPathPiece t = t `elem` map builtInAsType pathPieceTypes

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

-- | DateTime type, format: YYYY-MM-DD HH:MM:SS[Z|+XX|-XX] or YYYY-MM-DDTHH:MM:SS[Z|+XX|-XX]
tyDateTime :: BuiltIn
tyDateTime = builtIn "DateTime"

-- | Date type, format: YYYY-MM-DD
tyDate :: BuiltIn
tyDate = builtIn "Date"

-- | Time type, format: HH:MM:SS
tyTime :: BuiltIn
tyTime = builtIn "Time"
