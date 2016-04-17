{-# LANGUAGE OverloadedStrings #-}
module FTS.Convert
( convertCompilationUnit
, convertDef
, convertTypeExpr
, convertExpr
) where

import qualified FTS.AST as FTS
import qualified TS.AST as TS

convertCompilationUnit :: FTS.CompilationUnit -> TS.Program
convertCompilationUnit = (>>= convertDef)

convertDef :: FTS.Def -> [TS.Stmt]
convertDef (FTS.NamespaceDef name body) =
  [TS.NamespaceStmt name (body >>= convertDef)]
convertDef (FTS.TypeDef name type_) =
  [TS.TypeStmt name (convertTypeExpr type_)]
convertDef (FTS.ValueDef name value) =
  [TS.ConstStmt name (convertExpr value)]

convertTypeExpr :: FTS.TypeExpr -> TS.TypeExpr
convertTypeExpr (FTS.NumberTypeExpr) = TS.NumberTypeExpr
convertTypeExpr (FTS.InterfaceTypeExpr fields) =
  TS.ObjectTypeExpr (fmap convertInterfaceField fields)
  where convertInterfaceField (name, type_) = (name, convertTypeExpr type_)

convertExpr :: FTS.Expr -> TS.Expr
convertExpr (FTS.FieldLensExpr type_ field) =
  TS.ObjectExpr [("get", getter), ("set", setter)]
  where n = TS.NameExpr; (~.) = TS.MemberExpr; (~=) = TS.AssignExpr; (~$) = TS.CallExpr
        getter = TS.FunctionExpr [("c", Just $ convertTypeExpr type_)] Nothing
                                 (TS.Expr $ n "c" ~. field)
        setter = TS.FunctionExpr
                   [ ("c", Just $ convertTypeExpr type_)
                   , ("e", Just $ TS.TypeOfTypeExpr (n "c" ~. field))
                   ] Nothing
                   (TS.Stmts [ TS.ConstStmt "clone" (n "__clone" ~$ [n "c"])
                             , TS.ExprStmt ((n "clone" ~. field) ~= n "e")
                             , TS.ReturnStmt (n "clone")
                             ])