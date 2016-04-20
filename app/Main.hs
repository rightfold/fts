{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import FTS.Convert (convertCompilationUnit)
import FTS.Parse (compilationUnit)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (eof, parse)
import TS.Pretty (prettyProgram)

import qualified Data.Text.Lazy.IO as Text
import qualified Data.ByteString as ByteString

main :: IO ()
main = getArgs >>= \case
         [] -> do
           source <- ByteString.getContents
           goUTF8 "<stdin>" source
         [filename] -> do
           source <- ByteString.readFile filename
           goUTF8 filename source
         _ -> exit "usage: ftsc [filename]"

goUTF8 :: String -> ByteString -> IO ()
goUTF8 filename source' =
  case decodeUtf8' source' of
    Left unicodeError -> exit ("ftsc: unicode error: " ++ show unicodeError)
    Right source -> go filename source

go :: String -> Text -> IO ()
go filename source =
  let cu' = parse (compilationUnit <* eof) filename source
   in case cu' of
        Left e -> exit ("ftsc: parse error: " ++ show e)
        Right ast -> Text.putStrLn . prettyProgram $ convertCompilationUnit ast

exit :: String -> IO a
exit message = do
  hPutStrLn stderr message
  exitFailure
