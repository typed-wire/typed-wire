{-# LANGUAGE OverloadedStrings #-}
module TW.CodeGen.Haskell where

import TW.Ast
import qualified Data.Text as T

import Data.Monoid
import Data.Maybe

makeModule :: Module -> T.Text
makeModule m =
    T.unlines
    [ "module " <> printModuleName (m_name m) <> " where"
    , ""
    , T.intercalate "\n" (map makeImport $ m_imports m)
    , ""
    , T.intercalate "\n" (map makeTypeDef $ m_typeDefs m)
    ]

makeImport :: ModuleName -> T.Text
makeImport m =
    "import qualified " <> printModuleName m

makeTypeDef :: TypeDef -> T.Text
makeTypeDef td =
    case td of
      TypeDefEnum ed ->
          makeEnumDef ed
      TypeDefStruct sd ->
          makeStructDef sd

makeStructDef :: StructDef -> T.Text
makeStructDef sd =
    T.unlines
    [ "data " <> unTypeName (sd_name sd) <> " " <> T.intercalate " " (map unTypeVar $ sd_args sd)
    , "   = " <> unTypeName (sd_name sd)
    , "   { " <> T.intercalate "\n   , " (map makeStructField $ sd_fields sd)
    , "   } deriving (Show, Eq, Ord)"
    ]

makeEnumDef :: EnumDef -> T.Text
makeEnumDef ed =
    T.unlines
    [ "data " <> unTypeName (ed_name ed) <> " " <> T.intercalate " " (map unTypeVar $ ed_args ed)
    , "   = " <> T.intercalate "\n   | " (map makeEnumChoice $ ed_choices ed)
    , "     deriving (Show, Eq, Ord)"
    ]

makeEnumChoice :: EnumChoice -> T.Text
makeEnumChoice ec =
    (unChoiceName $ ec_name ec) <> fromMaybe "" (fmap ((<>) " !" . makeType) $ ec_arg ec)


makeStructField :: StructField -> T.Text
makeStructField sf =
    (unFieldName $ sf_name sf) <> " :: !" <> (makeType $ sf_type sf)

makeType :: Type -> T.Text
makeType t =
    case t of
      TyVar (TypeVar x) -> x
      TyCon qt args ->
          let ty = makeQualTypeName qt
          in case args of
               [] -> ty
               _ -> "(" <> ty <> " " <> T.intercalate " " (map makeType args) <> ")"

makeQualTypeName :: QualTypeName -> T.Text
makeQualTypeName qtn =
    case unModuleName $ qtn_module qtn of
      [] -> ty
      _ -> printModuleName (qtn_module qtn) <> "." <> ty
    where
      ty = unTypeName $ qtn_type qtn
