{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO (writeFile)
import LustreVerilog
import Options.Applicative
import Paths_LustreVerilog
import System.Exit (ExitCode (..), exitWith)
import Text.Pretty.Simple (pPrint)
import Prelude hiding (writeFile)

data Action = GetStdLibDir | ParsingStage | TypingStage | CompileAst | CompileStage Text

data CompilerOptions = CompilerOptions
  { compilerAction :: Action,
    outputFile :: FilePath,
    debug :: Bool,
    intSize :: Int,
    resetKind :: ResetKind,
    inputFile :: Maybe FilePath
  }

compilerOptionsParser :: Parser CompilerOptions
compilerOptionsParser =
  CompilerOptions
    <$> ( flag' GetStdLibDir (long "stdlib-dir" <> help "Get stdlib directory")
            <|> flag' ParsingStage (long "parse-only" <> help "Stop at parsing stage")
            <|> flag' TypingStage (long "type-only" <> help "Stop at typing stage")
            <|> flag' CompileAst (long "export-compiled" <> help "Stop before exporting to Verilog")
            <|> ( CompileStage
                    <$> strOption
                      ( long "main-node"
                          <> metavar "NODE"
                          <> help "Main node of the design"
                      )
                )
        )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output target file"
          <> showDefault
          <> value "out.v"
      )
    <*> switch (long "debug" <> short 'd' <> help "Enable debug mode")
    <*> option
      auto
      ( long "int-size"
          <> metavar "INT-SIZE"
          <> help "Bit size of a Lustre Integer"
          <> showDefault
          <> value 32
      )
    <*> ( flag' ActiveHigh (long "reset-low" <> help "Reset is active low")
            <|> flag' ActiveLow (long "reset-high" <> help "Reset is active high")
            <|> pure ActiveLow
        )
    <*> ( (Just <$> argument str (metavar "FILE" <> help "Input source file to compile"))
            <|> pure Nothing
        )

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
runCompiler opts =
  case compilerAction opts of
    GetStdLibDir -> printStdLib
    _ ->
      let inFile = fromMaybe (error "missing input file.") $ inputFile opts
       in do
            (path, txt) <- readInputFile inFile
            ast <- parseInput (intSize opts) path txt
            tast <- typeInput path txt ast
            case compilerAction opts of
              ParsingStage -> dumpIfDebug opts ast
              TypingStage -> dumpIfDebug opts tast
              CompileAst -> pPrint $ transformAst tast
              CompileStage mainModule ->
                maybe (putStrLn $ "There is no node: " <> unpack mainModule) (writeFile $ outputFile opts) $ compileInput opts mainModule tast

printStdLib :: IO ()
printStdLib = getDataDir >>= putStrLn

readInputFile :: String -> IO (String, Text)
readInputFile "-" = ("stdin",) . decodeUtf8 <$> B.getContents
readInputFile file = (file,) . decodeUtf8 <$> B.readFile file

parseInput :: Int -> FilePath -> Text -> IO PAst
parseInput defaultIntSize path txt = case parseFile defaultIntSize path txt of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 2)
  Right ast -> return ast

dumpIfDebug :: (Show a) => CompilerOptions -> a -> IO ()
dumpIfDebug opts ast = if debug opts then pPrint ast else putStrLn "No Error."

typeInput :: FilePath -> Text -> PAst -> IO TAst
typeInput path txt ast = case typeFile path txt ast of
  Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 3)
  Right tast -> return tast

compileInput :: CompilerOptions -> Text -> TAst -> Maybe Text
compileInput opt mainMod = toVerilog (resetKind opt) mainMod . transformAst