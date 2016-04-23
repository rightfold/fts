{-# LANGUAGE OverloadedStrings #-}
module FTS.Lex
( identifier

, stringLiteral

, kAlgebraic
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
, pSemicolon

, pHashGtGt
, pHashLtLt

, pHashPlusTilde
, pHashMinusTilde
, pHashAsteriskTilde
, pHashSlashTilde

, pLBrace
, pRBrace
, pLParen
, pRParen
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Text (Text)
import Text.Parsec ((<?>), char, choice, many, noneOf, notFollowedBy, oneOf, string, try, unexpected)
import Text.Parsec.Text (Parser)

import qualified Data.Text as Text

lexeme :: Parser a -> Parser a
lexeme = (<* many space)

space :: Parser ()
space = oneOf " \t\r\n" >> return ()

identifier :: Parser Text
identifier = (try binary <|> custom) <?> "identifier"
  where binary = kBinary >> (try pHashGtGt <|> pHashLtLt <|> pHashPlusTilde <|> pHashMinusTilde <|> pHashAsteriskTilde <|> pHashSlashTilde)
        custom = lexeme $ do
          name <- cons <$> identifierHead <*> many identifierTail
          when (name `elem` [ "algebraic"
                            , "binary"
                            , "fun"
                            , "namespace"
                            , "number"
                            , "type"
                            , "val"
                            ])
            (unexpected "keyword")
          return name
        cons c cs = Text.pack (c : cs)

identifierHead :: Parser Char
identifierHead = oneOf $ ['a'..'z'] ++ ['A' .. 'Z'] ++ "_$"

identifierTail :: Parser Char
identifierTail = identifierHead <|> oneOf ['0'..'9']

stringLiteral :: Parser Text
stringLiteral = lexeme $ Text.pack <$> (char '"' *> many (noneOf "\"") <* char '"')

k :: String -> Parser ()
k s = lexeme $ string s >> notFollowedBy identifierTail

kAlgebraic    = k "algebraic"
kBinary       = k "binary"
kFun          = k "fun"
kNamespace    = k "namespace"
kNumber       = k "number"
kType         = k "type"
kVal          = k "val"

p :: String -> Parser ()
p s = lexeme $ string s >> return ()

pDot                = p "."
pColon              = p ":"
pComma              = p ","
pEqual              = p "="
pSemicolon          = p ";"
pHash               = p "#" >> notFollowedBy (choice . map string $ [">>", "<<", "+~", "-~", "*~", "/~"])

pHashGtGt           = p "#>>" >> return ("binary$hash$gt$gt" :: Text)
pHashLtLt           = p "#<<" >> return ("binary$hash$lt$lt" :: Text)

pHashPlusTilde      = p "#+~" >> return ("binary$hash$plus$tilde" :: Text)
pHashMinusTilde     = p "#-~" >> return ("binary$hash$minus$tilde" :: Text)
pHashAsteriskTilde  = p "#*~" >> return ("binary$hash$asterisk$tilde" :: Text)
pHashSlashTilde     = p "#/~" >> return ("binary$hash$slash$tilde" :: Text)

pLBrace             = p "{"
pRBrace             = p "}"
pLParen             = p "("
pRParen             = p ")"
