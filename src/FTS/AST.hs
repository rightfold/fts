module FTS.AST where

import Data.Text (Text)

type CompilationUnit = [Def]

data Def
  = NamespaceDef Text [Def]
  | TypeDef Text TypeExpr
  | ValueDef Text Expr

data TypeExpr
  = NumberTypeExpr
  | InterfaceTypeExpr [(Text, TypeExpr)]

data Expr
  = FieldLensExpr TypeExpr Text
