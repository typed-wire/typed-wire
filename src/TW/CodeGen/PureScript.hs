{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TW.CodeGen.PureScript
    ( makeFileName, makeModule
    , libraryInfo
    )
where

import TW.Ast
import TW.BuiltIn
import TW.JsonRepr
import TW.Types
import TW.Utils

import Data.Maybe
import Data.Monoid
import System.FilePath
import qualified Data.List as L
import qualified Data.Text as T

libraryInfo :: LibraryInfo
libraryInfo = LibraryInfo "Purescript" "purescript-typed-wire" "0.2.0"

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
    , if not (null (m_apis m)) then "import Data.TypedWire.Api" else ""
    , ""
    , T.intercalate "\n" (map makeTypeDef $ m_typeDefs m)
    , T.intercalate "\n" (map makeApiDef $ m_apis m)
    ]

makeApiDef :: ApiDef -> T.Text
makeApiDef ad =
    T.unlines $
    catMaybes
    [ apiHeader
    , Just $ T.intercalate "\n" (map makeEndPoint (ad_endpoints ad))
    ]
    where
      apiHeader =
        case not (null (ad_headers ad)) of
          True -> Just $ makeHeaderType apiHeaderType (ad_headers ad)
          False -> Nothing
      apiCapitalized = capitalizeText (unApiName $ ad_name ad)
      handlerType = "ApiHandler" <> apiCapitalized
      apiHeaderType = handlerType <> "Headers"
      headerType ep = handlerType <> capitalizeText (unEndpointName (aed_name ep)) <> "Headers"
      makeHeaderName hdr = uncapitalizeText $ makeSafePrefixedFieldName (ah_name hdr)
      makeHeaderType ty headers =
        T.unlines
        [ "type " <> ty <> " = "
        , "    { " <> T.intercalate "\n    , " (map makeHeaderField headers)
        , "    }"
        ]
      makeHeaderField hdr =
        makeHeaderName hdr <> " :: String"
      makeEndPoint ep =
        T.unlines $
        catMaybes
        [ epHeader
        , Just $ funName <> " :: forall m. (Monad m) => "
          <> (maybe "" (const $ apiHeaderType <> " -> ") apiHeader)
          <> (maybe "" (const $ headerType ep <> " -> ") epHeader)
          <> urlParamSig
          <> (maybe "" (\t -> makeType t <> " -> ") $ aed_req ep)
          <> "ApiCall m " <> (maybe "Unit" makeType $ aed_req ep) <> " " <> makeType (aed_resp ep)
        , Just $ funName
          <> " "
          <> (maybe "" (const "apiHeaders ") apiHeader)
          <> (maybe "" (const "endpointHeaders ") epHeader)
          <> urlParams
          <> (maybe "" (const "reqBody ") $ aed_req ep)
          <> "runRequest = do"
        , Just $ "    let coreHeaders = [" <> T.intercalate ", " (map (headerPacker "apiHeaders") $ ad_headers ad) <> "]"
        , Just $ "    let fullHeaders = coreHeaders ++ [" <> T.intercalate ", " (map (headerPacker "endpointHeaders") $ aed_headers ep) <> "]"
        , Just $ "    let url = " <> T.intercalate " ++ \"/\" ++ " (map urlPacker routeInfo)
        , Just $ "    let method = " <> T.pack (show $ aed_verb ep)
        , Just $ "    let body = " <> (maybe "Nothing" (const "Just $ encodeJson reqBody") $ aed_req ep)
        , Just $ "    let req = { headers: fullHeaders, method: method, body: body, url: url }"
        , Just $ "    resp <- runRequest req"
        , Just $ "    return $ if (resp.statusCode /= 200) then Left \"Return code was not 200\" else decodeJson resp.body"
        ]
        where
          urlPacker (r, p) =
            case r of
              ApiRouteStatic t -> T.pack (show t)
              ApiRouteDynamic _ -> "toPathPiece p" <> T.pack (show p) <> ""
          headerPacker apiVar hdr =
             "{ key: " <> T.pack (show $ ah_name hdr) <> ", value: " <> apiVar <> "." <> makeHeaderName hdr <> " }"
          funName = unApiName (ad_name ad) <> capitalizeText (unEndpointName $ aed_name ep)
          routeInfo = zip (aed_route ep) ([0..] :: [Int])
          urlParams =
            T.concat $ flip mapMaybe routeInfo $ \(r,p) ->
            case r of
              ApiRouteStatic _ -> Nothing
              ApiRouteDynamic _ -> Just $ "p" <> T.pack (show p) <> " "
          urlParamSig =
            T.concat $ flip mapMaybe (aed_route ep) $ \r ->
            case r of
              ApiRouteStatic _ -> Nothing
              ApiRouteDynamic ty -> Just (makeType ty <> " -> ")
          epHeader =
            case not (null (aed_headers ep)) of
              True -> Just $ makeHeaderType (headerType ep) (aed_headers ep)
              False -> Nothing

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
      <> T.intercalate " ++ \", \" ++ " (map makeFieldShow (sd_fields sd))
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
                 Just _ -> "show (" <> constr <> " a) = " <> T.pack (show constr) <> " ++ \" \" ++ show a"
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
