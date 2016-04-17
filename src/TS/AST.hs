module TS.AST where

import Data.Text (Text)

type Program = [Stmt]

data Stmt
  = NamespaceStmt Text [Stmt]
  | TypeStmt Text TypeExpr
  | ConstStmt Text Expr
  | ReturnStmt Expr
  | ExprStmt Expr

data TypeExpr
  = NumberTypeExpr
  | NameTypeExpr Text
  | ObjectTypeExpr [(Text, TypeExpr)]
  | TypeOfTypeExpr Expr

data Expr
  = NameExpr Text
  | ObjectExpr [(Text, Expr)]
  | FunctionExpr [(Text, Maybe TypeExpr)] (Maybe TypeExpr) FunctionExprBody
  | MemberExpr Expr Text
  | CallExpr Expr [Expr]
  | AssignExpr Expr Expr

data FunctionExprBody
  = Expr Expr
  | Stmts [Stmt]
