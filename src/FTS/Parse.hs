module FTS.Parse
( compilationUnit
) where

import Control.Applicative ((<|>))
import FTS.Lex
import Text.Parsec (many)
import Text.Parsec.Text (Parser)

import qualified FTS.AST as FTS

compilationUnit :: Parser FTS.CompilationUnit
compilationUnit = many def

def :: Parser FTS.Def
def = namespaceDef <|> typeDef <|> valueDef

namespaceDef :: Parser FTS.Def
namespaceDef = do
  kNamespace
  name <- identifier
  pLBrace
  body <- many def
  pRBrace
  return $ FTS.NamespaceDef name body

typeDef :: Parser FTS.Def
typeDef = do
  kType
  name <- identifier
  pEqual
  type_ <- typeExpr
  pSemicolon
  return $ FTS.TypeDef name type_

valueDef :: Parser FTS.Def
valueDef = do
  kVal
  name <- identifier
  pEqual
  value <- expr
  pSemicolon
  return $ FTS.ValueDef name value


typeExpr :: Parser FTS.TypeExpr
typeExpr = numberTypeExpr <|> nameTypeExpr <|> interfaceTypeExpr

numberTypeExpr :: Parser FTS.TypeExpr
numberTypeExpr = kNumber >> return FTS.NumberTypeExpr

nameTypeExpr :: Parser FTS.TypeExpr
nameTypeExpr = FTS.NameTypeExpr <$> identifier

interfaceTypeExpr :: Parser FTS.TypeExpr
interfaceTypeExpr = do
  pLBrace
  fields <- many $ do
    name <- identifier
    pColon
    type_ <- typeExpr
    pComma
    return (name, type_)
  pRBrace
  return $ FTS.InterfaceTypeExpr fields

expr :: Parser FTS.Expr
expr = fieldLensExpr

fieldLensExpr :: Parser FTS.Expr
fieldLensExpr = do
  type_ <- typeExpr
  pHash
  name <- identifier
  return $ FTS.FieldLensExpr type_ name
