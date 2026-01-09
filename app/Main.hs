module Main (main) where

import Control.Exception (IOException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (writeFile)
import LustreVerilog
import Options.Applicative
import Paths_LustreVerilog (getDataDir)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (normalise)
import Text.Pretty.Simple (pPrint)
import Text.Printf (printf)
import Prelude hiding (writeFile)

data Action
  = GetStdLibDir
  | ParsingStage
  | TypingStage
  | CompileAst
  | CompileStage Text

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
            <|> flag' CompileAst (long "transform-only" <> help "Stop before exporting to Verilog")
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
    <*> optional (argument str (metavar "FILE" <> help "Input source file to compile"))

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
    _ -> do
      (path, fData) <- readInput opts
      ast <- parseInput (intSize opts) path fData
      case compilerAction opts of
        ParsingStage -> dumpIfDebug opts ast
        _ -> do
          tast <- typeInput path ast
          let cast = transformAst tast
          case compilerAction opts of
            TypingStage -> dumpIfDebug opts tast
            CompileAst -> dumpIfDebug opts cast
            CompileStage mainModule -> compileInput opts mainModule cast

printStdLib :: IO ()
printStdLib = getDataDir >>= putStrLn . normalise

exitWithError :: Int -> String -> IO a
exitWithError code err = putStrLn err >> exitWith (ExitFailure code)

dumpIfDebug :: (Show a) => CompilerOptions -> a -> IO ()
dumpIfDebug opts ast = if debug opts then pPrint ast else putStrLn "No Error."

readInput :: CompilerOptions -> IO (FilePath, ByteString)
readInput opts = case inputFile opts of
  Nothing -> exitWithError 2 "Missing input file"
  Just path -> do
    fData <-
      catch
        (if path == "-" then B.getContents else B.readFile path)
        ( \e -> do
            let err = show (e :: IOException)
            exitWithError 3 $ printf "Error: %s" err
        )
    return (path, fData)

parseInput :: Int -> FilePath -> ByteString -> IO PAst
parseInput defaultIntSize path txt =
  case parseFile defaultIntSize path txt of
    Left err -> exitWithError 1 err
    Right ast -> return ast

typeInput :: FilePath -> PAst -> IO TAst
typeInput path ast = case typeFile path ast of
  Left err -> exitWithError 1 err
  Right tast -> return tast

compileInput :: CompilerOptions -> Text -> CAst -> IO ()
compileInput opt mainMod cast = case toVerilog (resetKind opt) mainMod cast of
  Left err -> exitWithError 3 err
  Right out ->
    if outputFile opt == "-"
      then
        putStrLn $ T.unpack out
      else
        writeFile (outputFile opt) out
