{-# LANGUAGE OverloadedStrings #-}
module Main where

import FTS.Convert (convertCompilationUnit)
import FTS.Parse (compilationUnit)
import Text.Parsec (eof, parse)
import TS.Pretty (prettyProgram)

import qualified Data.Text.Lazy.IO as Text

main :: IO ()
main = do
  let src = "namespace FTS {\n\
            \    type Person = {age: number};\n\
            \    val blah = record #<< person #<< Person#age;\n\
            \    val halb = age #>> person #>> record;\n\
            \    namespace Nested {\n\
            \        val bad = number#quququ;\n\
            \    }\n\
            \}\n"
  let cu' = parse (compilationUnit <* eof) "main.fts" src
  case cu' of
    Left e -> print e
    Right ast -> Text.putStrLn . prettyProgram $ convertCompilationUnit ast

