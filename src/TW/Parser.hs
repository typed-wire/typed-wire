{-# LANGUAGE OverloadedStrings #-}
module TW.Parser
    ( moduleFromText
    , moduleFromFile
    )
where

import TW.Ast

import Control.Monad.Identity
import Data.Char
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec.Token as P

type Parser = Parsec T.Text ()

moduleFromText :: FilePath -> T.Text -> Either String Module
moduleFromText file t =
    case runParser parseModule () file t of
      Left err -> Left (show err)
      Right m -> Right m


moduleFromFile :: FilePath -> IO (Either String Module)
moduleFromFile file =
    do input <- T.readFile file
       return $ moduleFromText file input

parseModule :: Parser Module
parseModule =
    do reserved "module"
       moduleName <- parseModuleName
       _ <- semi
       imports <- many parseImport
       tyDefs <- many parseTypeDef
       return $ Module moduleName imports tyDefs

parseModuleName :: Parser ModuleName
parseModuleName =
    lexeme $
    ModuleName <$> dotSep1 parseCapitalized

parseImport :: Parser ModuleName
parseImport =
    do reserved "import"
       m <- parseModuleName
       _ <- semi
       return m

parseTypeDef :: Parser TypeDef
parseTypeDef =
    TypeDefEnum <$> parseEnumDef <|>
    TypeDefStruct <$> parseStructDef

parseDef :: (TypeName -> [TypeVar] -> [fld] -> def) -> String -> Parser fld -> Parser def
parseDef constr resName parseFields =
    do reserved resName
       name <- parseTypeName
       args <- parseTypeArgs
       fields <- braces $ many parseFields
       return $ constr name args fields

parseEnumDef :: Parser EnumDef
parseEnumDef = parseDef EnumDef "enum" parseEnumChoice

parseStructDef :: Parser StructDef
parseStructDef = parseDef StructDef "type" parseStructField

parseEnumChoice :: Parser EnumChoice
parseEnumChoice =
    do name <- parseChoiceName
       arg <- option Nothing (Just <$> parens parseType)
       _ <- semi
       return $ EnumChoice name arg

parseStructField :: Parser StructField
parseStructField =
    do name <- (FieldName . T.pack <$> identifier)
       reservedOp ":"
       ty <- parseType
       _ <- semi
       return $ StructField name ty

parseType :: Parser Type
parseType =
    TyVar <$> parseTypeVar <|>
    TyCon <$> parseQualTypeName <*> parseTyArgs
    where
      parseTyArgs =
          option [] $ parens $ commaSep1 parseType

parseTypeArgs :: Parser [TypeVar]
parseTypeArgs =
    option [] $
    angles $
    commaSep1 parseTypeVar

parseTypeVar :: Parser TypeVar
parseTypeVar = TypeVar . T.pack <$> identifier

parseTypeName :: Parser TypeName
parseTypeName = lexeme (TypeName <$> parseCapitalized)

parseQualTypeName :: Parser QualTypeName
parseQualTypeName =
    do md <- unModuleName <$> parseModuleName
       case reverse md of
         [] -> fail "This should never happen"
         (x:xs) ->
             return $ QualTypeName (ModuleName $ reverse xs) (TypeName x)

parseChoiceName :: Parser ChoiceName
parseChoiceName = lexeme (ChoiceName <$> parseCapitalized)

parseCapitalized :: Parser T.Text
parseCapitalized =
    do first <- satisfy isUpper
       rest <- many (alphaNum <|> char '_')
       return $ T.pack (first : rest)

languageDef :: P.GenLanguageDef T.Text st Identity
languageDef =
    P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = True
    , P.identStart = satisfy isLower
    , P.identLetter = alphaNum <|> oneOf "_"
    , P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedNames = ["module", "type", "enum", "import", "as"]
    , P.reservedOpNames = [":", "->"]
    , P.caseSensitive = False
    }

lexer :: P.GenTokenParser T.Text () Identity
lexer = P.makeTokenParser languageDef

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

angles :: Parser a -> Parser a
angles = P.angles lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

semi :: Parser String
semi = P.semi lexer

dot :: Parser String
dot = P.dot lexer

dotSep1 :: Parser a -> Parser [a]
dotSep1 p = sepBy p dot

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 lexer
