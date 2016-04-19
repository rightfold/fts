module FTS.AST where

import Data.Text (Text)

type CompilationUnit = [Def]

data Def
  = NamespaceDef Text [Def]
  | TypeDef Text TypeExpr
  | ValueDef Text Expr

data TypeExpr
  = NumberTypeExpr
  | NameTypeExpr Text
  | InterfaceTypeExpr [(Text, TypeExpr)]

data Expr
  = NameExpr Text
  | FieldLensExpr TypeExpr Text
  | CallExpr Expr [Expr]
