{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO (writeFile)
import ExportTyped (exportTyped)
import LustreVerilog
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import Text.Pretty.Simple (pPrint)
import Prelude hiding (writeFile)

data Stage = ParsingStage | TypingStage | ExportNormalised | CompileStage
  deriving (Show)

data CompilerOptions = CompilerOptions
  { stage :: Stage,
    outputFile :: FilePath,
    debug :: Bool,
    inputFile :: FilePath
  }
  deriving (Show)

compilerOptionsParser :: Parser CompilerOptions
compilerOptionsParser =
  CompilerOptions
    <$> ( flag' ParsingStage (long "parse-only" <> help "Stop at parsing stage")
            <|> flag' TypingStage (long "type-only" <> help "Stop at typing stage")
            <|> flag' ExportNormalised (long "export-normalised" <> help "Export the normalised input")
            <|> pure CompileStage
        )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output target file"
          <> showDefault
          <> value "a.out"
      )
    <*> switch (long "debug" <> short 'd' <> help "Enable debug mode")
    <*> argument str (metavar "FILE" <> help "Input source file to compile")

main :: IO ()
main = do
  options <- execParser opts
  runCompiler options
  where
    opts =
      info
        (compilerOptionsParser <**> helper)
        (fullDesc <> progDesc "A simple Lustre to Verilog Compiler")

runCompiler :: CompilerOptions -> IO ()
runCompiler opts = do
  (path, txt) <- readInputFile (inputFile opts)
  ast <- parseInput path txt
  tast <- typeInput path txt ast
  case stage opts of
    ParsingStage -> dumpIfDebug opts ast
    TypingStage -> dumpIfDebug opts tast
    ExportNormalised -> writeFile (outputFile opts) $ exportTyped tast
    CompileStage -> writeFile (outputFile opts) $ compileInput tast

readInputFile :: String -> IO (String, Text)
readInputFile "-" = ("stdin",) . decodeUtf8 <$> B.getContents
readInputFile file = (file,) . decodeUtf8 <$> B.readFile file

parseInput :: FilePath -> Text -> IO PAst
parseInput path txt = case parseFile path txt of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 2)
  Right ast -> return ast

dumpIfDebug :: (Show a) => CompilerOptions -> a -> IO ()
dumpIfDebug opts ast = if debug opts then pPrint ast else putStrLn "No Error."

typeInput :: FilePath -> Text -> PAst -> IO TAst
typeInput path txt ast = case typeFile path txt ast of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 3)
  Right tast -> return tast

compileInput :: TAst -> Text
compileInput = error "Not Implemented"