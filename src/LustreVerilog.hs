module LustreVerilog
  ( -- Parsing
    PAst (),
    parseFile,
    -- Typing
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

import Commons.Error (runCanFail)
import Compiling.Ast (CAst)
import Compiling.ToVerilog (toVerilogAst)
import Compiling.Transform (transformAst)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Parsing.Ast (PAst)
import Parsing.Grammar (parser)
import Parsing.ParsingMonad (ParsingParam (..), runParsingMonad)
import Typing.Ast (TAst)
import Typing.TypeNode (typeAst)
import Verilog.BaseLibrary (addRequirements)
import Verilog.Verilog (ResetKind (..), toVerilogCode)

parseFile :: Int -> FilePath -> ByteString -> Either String PAst
parseFile dIntSize file textData =
  let p = ParsingParam dIntSize
   in runCanFail file (runParsingMonad parser textData p)

typeFile :: FilePath -> PAst -> Either String TAst
typeFile file ast = runCanFail file (typeAst ast)

toVerilog :: ResetKind -> Text -> CAst -> Either String Text
toVerilog rst mainModule = toVerilogCode rst mainModule . addRequirements . toVerilogAst
