{-# LANGUAGE OverloadedStrings #-}
module FTS.Lex
( identifier

, stringLiteral

, kBinary
, kFun
, kNamespace
, kNumber
, kType
, kVal

, pDot
, pColon
, pComma
, pEqual
, pHash
, pHashGtGt
, pHashLtLt
, pSemicolon
, pLBrace
, pRBrace
, pLParen
, pRParen
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Text.Parsec (char, many, noneOf, notFollowedBy, oneOf, string, try)
import Text.Parsec.Text (Parser)

import qualified Data.Text as Text

lexeme :: Parser a -> Parser a
lexeme = (<* many space)

space :: Parser ()
space = oneOf " \t\r\n" >> return ()

identifier :: Parser Text
identifier = try binary <|> custom
  where binary = kBinary >> (try pHashGtGt <|> pHashLtLt)
        custom = lexeme $ cons <$> identifierHead <*> many identifierTail
        cons c cs = Text.pack (c : cs)

identifierHead :: Parser Char
identifierHead = oneOf $ ['a'..'z'] ++ ['A' .. 'Z'] ++ "_$"

identifierTail :: Parser Char
identifierTail = identifierHead <|> oneOf ['0'..'9']

stringLiteral :: Parser Text
stringLiteral = lexeme $ Text.pack <$> (char '"' *> many (noneOf "\"") <* char '"')

k :: String -> Parser ()
k s = lexeme $ string s >> notFollowedBy identifierTail

kBinary       = k "binary"
kFun          = k "fun"
kNamespace    = k "namespace"
kNumber       = k "number"
kType         = k "type"
kVal          = k "val"

p :: String -> Parser ()
p s = lexeme $ string s >> return ()

pDot          = p "."
pColon        = p ":"
pComma        = p ","
pEqual        = p "="
pHash         = p "#" >> notFollowedBy (string ">>" <|> string "<<")
pHashGtGt     = p "#>>" >> return ("binary$hash$gt$gt" :: Text)
pHashLtLt     = p "#<<" >> return ("binary$hash$lt$lt" :: Text)
pSemicolon    = p ";"
pLBrace       = p "{"
pRBrace       = p "}"
pLParen       = p "("
pRParen       = p ")"
