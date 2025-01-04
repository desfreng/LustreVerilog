module LustreVerilog
  ( -- Error Handling
    ShowErrorComponent (),
    ParseErrorBundle (),
    LustreVerilog.errorBundlePretty,
    -- Parsing
    PAst (),
    parseFile,
    -- Typing
    TypingError (),
    TAst (),
    typeFile,
  )
where

import Commons.TypingError (TypingError, runCanFail)
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Parsing.Ast (PAst)
import Parsing.Parser (pFile)
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, parse)
import qualified Text.Megaparsec.Error as MegaparsecError
import Typing.Ast (TAst)
import Typing.TypeNode (typeAst)

errorBundlePretty :: (ShowErrorComponent e) => ParseErrorBundle Text e -> String
errorBundlePretty = MegaparsecError.errorBundlePretty

parseFile :: FilePath -> Text -> Either (ParseErrorBundle Text Void) PAst
parseFile = parse pFile

typeFile :: FilePath -> Text -> PAst -> Either (ParseErrorBundle Text TypingError) TAst
typeFile file textData ast = runCanFail file textData (typeAst ast)