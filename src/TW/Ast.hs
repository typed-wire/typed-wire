{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TW.Ast where

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
   } deriving (Show, Eq)

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
