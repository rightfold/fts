{-# LANGUAGE OverloadedStrings #-}
module TS.Pretty
( prettyProgram
, prettyStmt
, prettyTypeExpr
, prettyExpr
) where

import Data.Monoid ((<>))
import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as Text
import qualified TS.AST as TS

indent :: Text -> Text
indent = Text.unlines . map ("    " `Text.append`) . Text.lines

prettyProgram :: TS.Program -> Text
prettyProgram = Text.concat . map prettyStmt

prettyStmt :: TS.Stmt -> Text
prettyStmt (TS.NamespaceStmt name body) =
  Text.concat [ "namespace "
              , Text.fromStrict name
              , " {\n"
              , indent $ Text.concat (map prettyStmt body)
              , "}\n"
              ]
prettyStmt (TS.TypeStmt name type_) =
  Text.concat [ "type "
              , Text.fromStrict name
              , " = "
              , prettyTypeExpr type_
              , ";\n"
              ]
prettyStmt (TS.ConstStmt name value) =
  Text.concat [ "const "
              , Text.fromStrict name
              , " = "
              , prettyExpr value
              , ";\n"
              ]
prettyStmt (TS.ReturnStmt expr) = Text.concat ["return ", prettyExpr expr, ";\n"]
prettyStmt (TS.ExprStmt expr) = Text.concat [prettyExpr expr, ";\n"]
prettyStmt (TS.ExportStmt stmt) = Text.concat ["export ", prettyStmt stmt]

prettyTypeExpr :: TS.TypeExpr -> Text
prettyTypeExpr TS.NumberTypeExpr = "number"
prettyTypeExpr (TS.NameTypeExpr name) = Text.fromStrict name
prettyTypeExpr (TS.ObjectTypeExpr fields) =
  Text.concat ["{\n", indent $ Text.concat (map prettyField fields), "}"]
  where prettyField (name, type_) =
          Text.concat [Text.fromStrict name, ": ", prettyTypeExpr type_, ",\n"]
prettyTypeExpr (TS.TypeOfTypeExpr expr) =
  Text.concat ["typeof ", prettyExpr expr]

prettyExpr :: TS.Expr -> Text
prettyExpr (TS.CommaExpr e es) =
  Text.concat ["(", Text.intercalate ", " (map prettyExpr (e : es)), ")"]
prettyExpr (TS.NameExpr name) = Text.fromStrict name
prettyExpr (TS.StringExpr value) = "\"" <> Text.fromStrict value <> "\""
prettyExpr (TS.ObjectExpr fields) =
  Text.concat ["{\n", indent $ Text.concat (map prettyField fields), "}"]
  where prettyField (name, value) =
          Text.concat [Text.fromStrict name, ": ", prettyExpr value, ",\n"]
prettyExpr (TS.FunctionExpr params returnType body) =
  Text.concat [ "("
              , Text.intercalate ", " (map prettyParam params)
              , ") => " -- TODO: return type
              , prettyBody body
              ]
  where prettyParam (name, Just type_) =
          Text.concat [Text.fromStrict name, ": ", prettyTypeExpr type_]
        prettyParam (name, Nothing) = Text.fromStrict name
        prettyBody (TS.Expr expr) =
          let text = prettyExpr expr
           in if Text.head text == '{'
              then Text.concat ["(", text, ")"]
              else text
        prettyBody (TS.Stmts stmts) =
          Text.concat ["{\n", indent $ Text.concat (map prettyStmt stmts), "}"]
prettyExpr (TS.MemberExpr object member) =
  Text.concat [parens 18 object, ".", Text.fromStrict member]
prettyExpr (TS.CallExpr callee arguments) =
  Text.concat [ parens 17 callee
              , "("
              , Text.intercalate ", " (map prettyExpr arguments)
              , ")"
              ]
prettyExpr (TS.AssignExpr target source) =
  Text.concat [parens 3 target, " = ", parens 3 source]

parens :: Int -> TS.Expr -> Text
parens n e | precedence e < n = Text.concat ["(", prettyExpr e, ")"]
           | otherwise = prettyExpr e

precedence :: TS.Expr -> Int
precedence (TS.NameExpr _) = 20
precedence (TS.ObjectExpr _) = 20
precedence (TS.FunctionExpr _ _ _) = (-1)
precedence (TS.MemberExpr _ _) = 18
precedence (TS.CallExpr _ _) = 17
precedence (TS.AssignExpr _ _) = 3
