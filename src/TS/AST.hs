module TS.AST where

import Data.Text (Text)

type Program = [Stmt]

data Stmt
  = NamespaceStmt Text [Stmt]
  | TypeStmt Text TypeExpr
  | ConstStmt Text Expr
  | ReturnStmt Expr
  | ExprStmt Expr
  | ExportStmt Stmt
  | ClassStmt Text [(Text, [(Text, Maybe TypeExpr)], (Maybe TypeExpr), [Stmt])]

data TypeExpr
  = NumberTypeExpr
  | NameTypeExpr Text
  | ObjectTypeExpr [(Text, TypeExpr)]
  | TypeOfTypeExpr Expr
  | UnionTypeExpr TypeExpr TypeExpr

data Expr
  = NameExpr Text
  | StringExpr Text
  | ObjectExpr [(Text, Expr)]
  | FunctionExpr [(Text, Maybe TypeExpr)] (Maybe TypeExpr) FunctionExprBody
  | MemberExpr Expr Text
  | CallExpr Expr [Expr]
  | AssignExpr Expr Expr
  | NewExpr Expr [Expr]

data FunctionExprBody
  = Expr Expr
  | Stmts [Stmt]
