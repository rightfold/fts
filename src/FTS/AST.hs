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
  = BlockExpr [Expr]
  | NameExpr Text
  | StringExpr Text
  | CallExpr Expr [Expr]
  | MemberExpr Expr Text
  | FunctionExpr [(Text, Maybe TypeExpr)] (Maybe TypeExpr) Expr
  | FieldLensExpr TypeExpr Text
