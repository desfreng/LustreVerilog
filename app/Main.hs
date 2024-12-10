{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Parsing.Parser
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)
import Typing.TypeChecker

main :: IO ()
main = do
  args <- getArgs
  (path, txt) <- readArgs args
  ast <- parseFile path txt
  tast <- typeFile path txt ast
  pPrint tast
  where
    readArgs [] = ("stdin",) . decodeUtf8 <$> B.getContents
    readArgs (f : _) = (f,) . decodeUtf8 <$> B.readFile f

parseFile :: FilePath -> Text -> IO Ast
parseFile path txt = case parse pFile path txt of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 2)
  Right ast -> return ast

typeFile :: FilePath -> Text -> Ast -> IO TAst
typeFile path txt ast = case typeAst path txt ast of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 3)
  Right tast -> return tast
