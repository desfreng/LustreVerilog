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
    -- Compilation
    CAst (),
    transformAst,
    -- Verilog Synthesis
    ResetKind (..),
    toVerilog,
  )
where

import Commons.TypingError (TypingError, runCanFail)
import Compiling.Ast (CAst)
import Compiling.ToVerilog (toVerilogAst)
import Compiling.Transform (transformAst)
import Control.Monad.Reader (runReader)
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Parsing.Ast (PAst)
import Parsing.Parser (pFile)
import safe Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, runParserT)
import qualified Text.Megaparsec.Error as MegaparsecError
import Typing.Ast (TAst)
import Typing.TypeNode (typeAst)
import Verilog.BaseLibrary (addRequirements)
import Verilog.Verilog (ResetKind (..), toVerilogCode)

errorBundlePretty :: (ShowErrorComponent e) => ParseErrorBundle Text e -> String
errorBundlePretty = MegaparsecError.errorBundlePretty

parseFile :: Int -> FilePath -> Text -> Either (ParseErrorBundle Text Void) PAst
parseFile defaultIntSize file textData = runReader (runParserT pFile file textData) defaultIntSize

typeFile :: FilePath -> Text -> PAst -> Either (ParseErrorBundle Text TypingError) TAst
typeFile file textData ast = runCanFail file textData (typeAst ast)

toVerilog :: ResetKind -> Text -> CAst -> Maybe Text
toVerilog rst mainModule = toVerilogCode rst mainModule . addRequirements . toVerilogAst