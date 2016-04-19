module FTS.Parse
( compilationUnit
) where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Text (Text)
import FTS.Lex
import Text.Parsec (many, sepBy, optionMaybe, try)
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
  fields <- (`sepBy` pComma) $ do
    name <- identifier
    pColon
    type_ <- typeExpr
    return (name, type_)
  pRBrace
  return $ FTS.InterfaceTypeExpr fields

expr :: Parser FTS.Expr
expr = composeExpr

composeExpr :: Parser FTS.Expr
composeExpr = binop RightAssoc (try pHashGtGt <|> pHashLtLt) callExpr

data Assoc = RightAssoc

binop :: Assoc -> Parser Text -> Parser FTS.Expr -> Parser FTS.Expr
binop RightAssoc op next = do
  l <- next
  r' <- optionMaybe $ (,) <$> op <*> binop RightAssoc op next
  return $ case r' of
    Just (o, r) -> FTS.CallExpr (FTS.NameExpr o) [l, r]
    Nothing -> l

callExpr :: Parser FTS.Expr
callExpr = foldl' FTS.CallExpr <$> primaryExpr <*> many argumentList
  where argumentList = do
          pLParen
          arguments <- expr `sepBy` pComma
          pRParen
          return arguments

primaryExpr :: Parser FTS.Expr
primaryExpr = try fieldLensExpr <|> nameExpr

fieldLensExpr :: Parser FTS.Expr
fieldLensExpr = do
  type_ <- typeExpr
  pHash
  name <- identifier
  return $ FTS.FieldLensExpr type_ name

nameExpr :: Parser FTS.Expr
nameExpr = FTS.NameExpr <$> identifier
