{-# LANGUAGE OverloadedStrings #-}
module Main where

import FTS.Convert (convertCompilationUnit)
import TS.Pretty (prettyProgram)

import qualified Data.Text.Lazy.IO as Text
import qualified FTS.AST as FTS

main :: IO ()
main = Text.putStrLn . prettyProgram $ convertCompilationUnit cu
  where cu = [ftsNs]
        ftsNs = FTS.NamespaceDef "FTS" [age]
        age = FTS.ValueDef "age" (FTS.FieldLensExpr personTE "age")
        personTE = FTS.InterfaceTypeExpr [("age", FTS.NumberTypeExpr)]
