{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import LustreVerilog
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  (path, txt) <- readArgs args
  ast <- parseInput path txt
  tast <- typeInput path txt ast
  pPrint tast
  where
    readArgs [] = ("stdin",) . decodeUtf8 <$> B.getContents
    readArgs (f : _) = (f,) . decodeUtf8 <$> B.readFile f

parseInput :: FilePath -> Text -> IO PAst
parseInput path txt = case parseFile path txt of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 2)
  Right ast -> return ast

typeInput :: FilePath -> Text -> PAst -> IO TAst
typeInput path txt ast = case typeFile path txt ast of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 3)
  Right tast -> return tast
