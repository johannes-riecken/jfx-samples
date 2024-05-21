{-# OPTIONS_GHC -Wall #-}
module MaxDataParser where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DataTypes

type Parser = Parsec Void String

lexeme :: Parser String -> Parser String
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

classDef :: Parser ClassDef
classDef = choice
    [ try simpleClassDef
    ]

simpleClassDef :: Parser ClassDef
simpleClassDef = do
    _ <- symbol "public" *> optional (symbol "static") *> symbol "class"
    cn <- className
    super <- optional $ symbol "extends" *> className
    (cds, fds) <- braces $ (,) <$> many classDef <*> many fieldDef
    pure $ SimpleClassDef cn super cds fds

fieldDef :: Parser FieldDef
fieldDef = choice
    [ try simpleFieldDef
    , try arrayFieldDef
    , try mapFieldDef
    , try listFieldDef
    ]

simpleFieldDef :: Parser FieldDef
simpleFieldDef = do
    _ <- symbol "public"
    tn <- typeName
    fn <- fieldName
    _ <- symbol ";"
    pure $ SimpleFieldDef tn fn

arrayFieldDef :: Parser FieldDef
arrayFieldDef = do
    _ <- symbol "public"
    tn <- typeName
    _ <- symbol "[]"
    fn <- fieldName
    _ <- symbol ";"
    pure $ ArrayFieldDef tn fn

mapFieldDef :: Parser FieldDef
mapFieldDef = do
    _ <- symbol "public" *> symbol "Map" *> symbol "<"
    k <- typeName <* symbol ","
    v <- typeName <* symbol ">"
    fn <- fieldName
    _ <- symbol "=" *> symbol "new" *> symbol "HashMap<>()" *> symbol ";"
    pure $ MapFieldDef k v fn

listFieldDef :: Parser FieldDef
listFieldDef = do
    _ <- symbol "public" *> symbol "List" *> symbol "<"
    tn <- typeName <* symbol ">"
    fn <- fieldName <* symbol ";"
    pure $ ListFieldDef tn fn

className :: Parser ClassName
className = lexeme $ (:) <$> upperChar <*> many alphaNumChar

typeName :: Parser TypeName
typeName = lexeme $ some alphaNumChar

fieldName :: Parser FieldName
fieldName = lexeme $ (:) <$> lowerChar <*> many alphaNumChar

parseClassDef :: String -> Either String ClassDef
parseClassDef input = case parse (classDef <* eof) "" input of
    Left bundle -> Left (errorBundlePretty bundle)
    Right x -> Right x
