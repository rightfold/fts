{-# LANGUAGE OverloadedStrings #-}
module FTS.Convert
( convertCompilationUnit
, convertDef
, convertTypeExpr
, convertExprToStmts
, convertExprToExpr
) where

import qualified FTS.AST as FTS
import qualified TS.AST as TS

convertCompilationUnit :: FTS.CompilationUnit -> TS.Program
convertCompilationUnit = (>>= convertDef)

convertDef :: FTS.Def -> [TS.Stmt]
convertDef (FTS.NamespaceDef name body) =
  [TS.NamespaceStmt name (body >>= convertDef)]
convertDef (FTS.TypeDef name type_) =
  [TS.ExportStmt $ TS.TypeStmt name (convertTypeExpr type_)]
convertDef (FTS.ValueDef name value) =
  [TS.ExportStmt $ TS.ConstStmt name (convertExprToExpr value)]

convertTypeExpr :: FTS.TypeExpr -> TS.TypeExpr
convertTypeExpr (FTS.NumberTypeExpr) = TS.NumberTypeExpr
convertTypeExpr (FTS.NameTypeExpr name) = TS.NameTypeExpr name
convertTypeExpr (FTS.InterfaceTypeExpr fields) =
  TS.ObjectTypeExpr (fmap convertInterfaceField fields)
  where convertInterfaceField (name, type_) = (name, convertTypeExpr type_)

convertExprToStmts :: (TS.Expr -> TS.Stmt) -> FTS.Expr -> [TS.Stmt]
convertExprToStmts wrap (FTS.BlockExpr es@(_ : _)) =
  (init es >>= convertExprToStmts TS.ExprStmt)
  ++ [wrap . convertExprToExpr $ last es]
convertExprToStmts wrap e = [wrap (convertExprToExpr e)]

convertExprToExpr :: FTS.Expr -> TS.Expr
convertExprToExpr (FTS.BlockExpr []) = TS.NameExpr "__undefined"
convertExprToExpr (FTS.BlockExpr [e]) = convertExprToExpr e
convertExprToExpr (FTS.NameExpr name) = TS.NameExpr name
convertExprToExpr (FTS.StringExpr value) = TS.StringExpr value
convertExprToExpr (FTS.CallExpr callee arguments) =
  TS.CallExpr (convertExprToExpr callee) (map convertExprToExpr arguments)
convertExprToExpr (FTS.MemberExpr object member) =
  TS.MemberExpr (convertExprToExpr object) member
convertExprToExpr (FTS.FunctionExpr [] returnType body) =
  TS.FunctionExpr [] -- TODO: params
                  (convertTypeExpr <$> returnType)
                  (TS.Stmts $ convertExprToStmts TS.ReturnStmt body)
convertExprToExpr (FTS.FieldLensExpr type_ field) =
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
convertExprToExpr e = iife (convertExprToStmts TS.ReturnStmt e)

iife :: [TS.Stmt] -> TS.Expr
iife ss = TS.CallExpr (TS.FunctionExpr [] Nothing (TS.Stmts ss)) []
