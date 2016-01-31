{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TW.CodeGen.PureScript
    ( makeFileName, makeModule
    , libraryInfo
    )
where

{-
TODO:
- support api defs
-}

import TW.Ast
import TW.BuiltIn
import TW.JsonRepr
import TW.Types

import Data.Maybe
import Data.Monoid
import System.FilePath
import qualified Data.List as L
import qualified Data.Text as T

libraryInfo :: LibraryInfo
libraryInfo = LibraryInfo "purescript-typed-wire" "3710937a424ce442feecd5f339af223744c55292"

makeFileName :: ModuleName -> FilePath
makeFileName (ModuleName parts) =
    (L.foldl' (</>) "" $ map T.unpack parts) ++ ".purs"

makeModule :: Module -> T.Text
makeModule m =
    T.unlines
    [ "module " <> printModuleName (m_name m) <> " where"
    , ""
    , T.intercalate "\n" (map makeImport $ m_imports m)
    , ""
    , "import Data.TypedWire.Prelude"
    , ""
    , T.intercalate "\n" (map makeTypeDef $ m_typeDefs m)
    ]

makeImport :: ModuleName -> T.Text
makeImport m =
    "import qualified " <> printModuleName m <> " as " <> printModuleName m

makeTypeDef :: TypeDef -> T.Text
makeTypeDef td =
    case td of
      TypeDefEnum ed ->
          makeEnumDef ed
      TypeDefStruct sd ->
          makeStructDef sd

decoderName :: TypeName -> T.Text
decoderName ty = "dec" <> unTypeName ty

encoderName :: TypeName -> T.Text
encoderName ty = "enc" <> unTypeName ty

eqName :: TypeName -> T.Text
eqName ty = "eq" <> unTypeName ty

showName :: TypeName -> T.Text
showName ty = "show" <> unTypeName ty

makeStructDef :: StructDef -> T.Text
makeStructDef sd =
    T.unlines
    [ "data " <> fullType
    , "   = " <> unTypeName (sd_name sd)
    , "   { " <> T.intercalate "\n   , " (map makeStructField $ sd_fields sd)
    , "   }"
    , ""
    , "instance " <> eqName (sd_name sd) <> " :: "
      <> tcPreds (sd_args sd) ["Eq"] <> "Eq (" <> fullType <> ") where "
      <> "eq (" <> justType <> " a) (" <> justType <> " b) = "
      <> T.intercalate " && " (map makeFieldEq (sd_fields sd))
    , "instance " <> showName (sd_name sd) <> " :: "
      <> tcPreds (sd_args sd) ["Show"] <> "Show (" <> fullType <> ") where "
      <> "show (" <> justType <> " a) = " <> T.pack (show justType) <> " ++ \"{\" ++ "
      <> T.intercalate " ++ " (map makeFieldShow (sd_fields sd))
      <> " ++ \"}\""
    , "instance " <> encoderName (sd_name sd) <> " :: "
      <> tcPreds (sd_args sd) ["EncodeJson"] <> "EncodeJson" <> " (" <> fullType <> ") where"
    , "    encodeJson (" <> unTypeName (sd_name sd) <> " objT) ="
    , "        " <> T.intercalate "\n         ~> " (map makeToJsonFld $ sd_fields sd)
    , "        ~> jsonEmptyObject"
    , "instance " <> decoderName (sd_name sd) <> " :: "
      <> tcPreds (sd_args sd) ["DecodeJson"] <> "DecodeJson" <> " (" <> fullType <> ") where"
    , "    decodeJson jsonT = do"
    , "        objT <- decodeJson jsonT"
    , "        " <> T.intercalate "\n        " (map makeFromJsonFld $ sd_fields sd)
    , "        pure $ " <> unTypeName (sd_name sd) <> " { " <> T.intercalate ", " (map makeFieldSetter $ sd_fields sd) <> " }"
    ]
    where
      makeFieldShow fld =
          let name = unFieldName $ sf_name fld
          in  T.pack (show name) <> " ++ \": \" ++ show a." <> name
      makeFieldEq fld =
          let name = unFieldName $ sf_name fld
          in  "a." <> name <> " == " <> "b." <> name
      makeFieldSetter fld =
          let name = unFieldName $ sf_name fld
          in name <> " : " <> "v" <> name
      makeFromJsonFld fld =
          let name = unFieldName $ sf_name fld
          in case sf_type fld of
               (TyCon q _) | q == bi_name tyMaybe ->
                  "v" <> name <> " <- objT .?? " <> T.pack (show name)
               _ ->
                  "v" <> name <> " <- objT .? " <> T.pack (show name)
      makeToJsonFld fld =
          let name = unFieldName $ sf_name fld
          in T.pack (show name) <> " " <> ":=" <> " objT." <> name
      justType = unTypeName (sd_name sd)
      fullType =
          unTypeName (sd_name sd) <> " " <> T.intercalate " " (map unTypeVar $ sd_args sd)

makeStructField :: StructField -> T.Text
makeStructField sf =
    unFieldName (sf_name sf) <> " :: " <> makeType (sf_type sf)

tcPreds :: [TypeVar] -> [T.Text] -> T.Text
tcPreds args tyClasses =
    if null args
    then ""
    else let mkPred (TypeVar tv) =
                 T.intercalate "," $ flip map tyClasses $ \tyClass -> tyClass <> " " <> tv
         in "(" <> T.intercalate "," (map mkPred args) <> ") => "

makeEnumDef :: EnumDef -> T.Text
makeEnumDef ed =
    T.unlines
    [ "data " <> fullType
    , "   = " <> T.intercalate "\n   | " (map makeEnumChoice $ ed_choices ed)
    , ""
    , "instance " <> eqName (ed_name ed) <> " :: "
      <> tcPreds (ed_args ed) ["Eq"] <> "Eq (" <> fullType <> ") where "
    , "    " <> T.intercalate "\n    " (map makeChoiceEq $ ed_choices ed)
    , "    eq _ _ = false"
    , "instance " <> showName (ed_name ed) <> " :: "
      <> tcPreds (ed_args ed) ["Show"] <> "Show (" <> fullType <> ") where "
    , "    " <> T.intercalate "\n    " (map makeChoiceShow $ ed_choices ed)
    , "instance " <> encoderName (ed_name ed) <> " :: "
      <> tcPreds (ed_args ed) ["EncodeJson"] <> "EncodeJson" <> " (" <> fullType <> ") where"
    , "    encodeJson x ="
    , "        case x of"
    , "          " <> T.intercalate "\n          " (map mkToJsonChoice $ ed_choices ed)
    , "instance " <> decoderName (ed_name ed) <> " :: "
      <> tcPreds (ed_args ed) ["DecodeJson"] <> "DecodeJson" <> " (" <> fullType <> ") where"
    , "    decodeJson jsonT ="
    , "        decodeJson jsonT >>= \\objT -> "
    , "        " <> T.intercalate "\n        <|> " (map mkFromJsonChoice $ ed_choices ed)
    ]
    where
      makeChoiceShow ec =
          let constr = unChoiceName $ ec_name ec
          in case ec_arg ec of
                 Nothing -> "show (" <> constr <> ") = " <> T.pack (show constr)
                 Just _ -> "show (" <> constr <> " a) = " <> T.pack (show constr) <> " ++ show a"
      makeChoiceEq ec =
          let constr = unChoiceName $ ec_name ec
          in case ec_arg ec of
                 Nothing -> "eq (" <> constr <> ") (" <> constr <> ") = true"
                 Just _ -> "eq (" <> constr <> " a) (" <> constr <> " b) = a == b"
      mkFromJsonChoice ec =
          let constr = unChoiceName $ ec_name ec
              tag = camelTo2 '_' $ T.unpack constr
              (op, opEnd) =
                  case ec_arg ec of
                    Nothing -> ("<$ (eatBool <$> (", "))")
                    Just _ -> ("<$>", "")
          in "(" <> constr <> " " <> op <> " objT " <> ".?" <> " " <> T.pack (show tag) <> opEnd <> ")"
      mkToJsonChoice ec =
          let constr = unChoiceName $ ec_name ec
              tag = camelTo2 '_' $ T.unpack constr
              (argParam, argVal) =
                  case ec_arg ec of
                    Nothing -> ("", "true")
                    Just _ -> ("y", "y")
          in constr <> " " <> argParam <> " -> "
             <> " " <> T.pack (show tag) <> " " <> " := " <> " " <> argVal <> " ~> jsonEmptyObject"
      fullType =
          unTypeName (ed_name ed) <> " " <> T.intercalate " " (map unTypeVar $ ed_args ed)

makeEnumChoice :: EnumChoice -> T.Text
makeEnumChoice ec =
    (unChoiceName $ ec_name ec) <> fromMaybe "" (fmap ((<>) " " . makeType) $ ec_arg ec)

makeType :: Type -> T.Text
makeType t =
    case isBuiltIn t of
      Nothing ->
          case t of
            TyVar (TypeVar x) -> x
            TyCon qt args ->
                let ty = makeQualTypeName qt
                in case args of
                     [] -> ty
                     _ -> "(" <> ty <> " " <> T.intercalate " " (map makeType args) <> ")"
      Just (bi, tvars)
          | bi == tyString -> "String"
          | bi == tyInt -> "Int"
          | bi == tyBool -> "Boolean"
          | bi == tyFloat -> "Number"
          | bi == tyMaybe -> "(Maybe " <> T.intercalate " " (map makeType tvars) <> ")"
          | bi == tyBytes -> "AsBase64"
          | bi == tyList -> "(Array " <> T.intercalate " " (map makeType tvars) <> ")"
          | bi == tyDateTime -> "DateTime"
          | bi == tyTime -> "TimeOfDay"
          | bi == tyDate -> "Day"
          | otherwise ->
              error $ "Haskell: Unimplemented built in type: " ++ show t

makeQualTypeName :: QualTypeName -> T.Text
makeQualTypeName qtn =
    case unModuleName $ qtn_module qtn of
      [] -> ty
      _ -> printModuleName (qtn_module qtn) <> "." <> ty
    where
      ty = unTypeName $ qtn_type qtn
