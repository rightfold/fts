module FTS.Lex
( identifier

, kNamespace
, kNumber
, kType
, kVal

, pColon
, pComma
, pEqual
, pHash
, pSemicolon
, pLBrace
, pRBrace
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Text.Parsec (many, notFollowedBy, oneOf, string)
import Text.Parsec.Text (Parser)

import qualified Data.Text as Text

lexeme :: Parser a -> Parser a
lexeme = (<* many space)

space :: Parser ()
space = oneOf " \t\r\n" >> return ()

identifier :: Parser Text
identifier = lexeme $ cons <$> identifierHead <*> many identifierTail
  where cons c cs = Text.pack (c : cs)

identifierHead :: Parser Char
identifierHead = oneOf $ ['a'..'z'] ++ ['A' .. 'Z'] ++ "_$"

identifierTail :: Parser Char
identifierTail = identifierHead <|> oneOf ['0'..'9']

k :: String -> Parser ()
k s = lexeme $ string s >> notFollowedBy identifierTail

kNamespace    = k "namespace"
kNumber       = k "number"
kType         = k "type"
kVal          = k "val"

p :: String -> Parser ()
p s = lexeme $ string s >> return ()

pColon        = p ":"
pComma        = p ","
pEqual        = p "="
pHash         = p "#"
pSemicolon    = p ";"
pLBrace       = p "{"
pRBrace       = p "}"
