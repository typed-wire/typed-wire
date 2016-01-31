{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TW.Ast where

import Network.HTTP.Types.Method
import qualified Data.Text as T

newtype ModuleName
    = ModuleName { unModuleName :: [T.Text] }
      deriving (Show, Eq, Ord)

printModuleName :: ModuleName -> T.Text
printModuleName (ModuleName comps) = T.intercalate "." comps

printModuleNameS :: ModuleName -> String
printModuleNameS = T.unpack . printModuleName

newtype TypeName
    = TypeName { unTypeName :: T.Text }
      deriving (Show, Eq, Ord)

newtype TypeVar
    = TypeVar { unTypeVar :: T.Text }
      deriving (Show, Eq, Ord)

newtype FieldName
    = FieldName { unFieldName :: T.Text }
      deriving (Show, Eq, Ord)

newtype ChoiceName
    = ChoiceName { unChoiceName :: T.Text }
      deriving (Show, Eq, Ord)

newtype ApiName
    = ApiName { unApiName :: T.Text }
      deriving (Show, Eq, Ord)

newtype EndpointName
    = EndpointName { unEndpointName :: T.Text }
      deriving (Show, Eq, Ord)

data QualTypeName
   = QualTypeName
   { qtn_module :: ModuleName
   , qtn_type :: TypeName
   } deriving (Show, Eq, Ord)

data Module
   = Module
   { m_name :: ModuleName
   , m_imports :: [ModuleName]
   , m_typeDefs :: [TypeDef]
   , m_apis :: [ApiDef]
   } deriving (Show, Eq)

data ApiDef
   = ApiDef
   { ad_name :: ApiName
   , ad_headers :: [ApiHeader]
   , ad_endpoints :: [ApiEndpointDef]
   } deriving (Show, Eq)

data ApiEndpointDef
   = ApiEndpointDef
   { aed_name :: EndpointName
   , aed_verb :: StdMethod
   , aed_route :: [ApiRouteComp]
   , aed_headers :: [ApiHeader]
   , aed_req :: Maybe Type
   , aed_resp :: Type
   } deriving (Show, Eq)

data ApiRouteComp
   = ApiRouteStatic T.Text
   | ApiRouteDynamic Type
     deriving (Show, Eq)

data ApiHeader
   = ApiHeader
   { ah_name :: T.Text
   , ah_value :: ApiHeaderValue
   } deriving (Show, Eq)

data ApiHeaderValue
   = ApiHeaderValueStatic T.Text
   | ApiHeaderValueDynamic
     deriving (Show, Eq)

data TypeDef
   = TypeDefEnum EnumDef
   | TypeDefStruct StructDef
     deriving (Show, Eq)

data EnumDef
   = EnumDef
   { ed_name :: TypeName
   , ed_args :: [TypeVar]
   , ed_choices :: [EnumChoice]
   } deriving (Show, Eq)

data EnumChoice
   = EnumChoice
   { ec_name :: ChoiceName
   , ec_arg :: Maybe Type
   } deriving (Show, Eq)

data StructDef
   = StructDef
   { sd_name :: TypeName
   , sd_args :: [TypeVar]
   , sd_fields :: [StructField]
   } deriving (Show, Eq)

data StructField
   = StructField
   { sf_name :: FieldName
   , sf_type :: Type
   } deriving (Show, Eq)

data Type
   = TyVar TypeVar
   | TyCon QualTypeName [Type]
   deriving (Show, Eq)


{-
module Basic;

type Foo {
   someField : Int;
   someOtherField : Maybe Int;
}

type Bar<T> {
   someField : T;
   moreFields : Int;
}

enum Psy {
   Simple;
   Tagged(Foo);
}
-}
