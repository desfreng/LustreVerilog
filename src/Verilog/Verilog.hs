{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Verilog.Verilog (ResetKind (..), toVerilogCode) where

import Commons.Ids
import Commons.Types (BVSize (BVSize))
import Compiling.ToVerilog (verilogName)
import Control.Monad.State
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Prettyprinter
import Prettyprinter.Render.Text
import Verilog.Ast

bodyIdent :: Int
bodyIdent = 2

declIdent :: Int
declIdent = 4

layoutOptions :: LayoutOptions
layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine 80 1.0}

data ResetKind = ActiveLow | ActiveHigh

type CallNumbering = State Int

freshId :: CallNumbering Int
freshId = state $ \s -> (s, s + 1)

concatModule :: [Doc ann] -> Doc ann
concatModule =
  let twoLines = hardline <> hardline
   in concatWith (\x y -> x <> twoLines <> y)

concatExpr :: [Doc ann] -> Doc ann
concatExpr l = vsep $ (<> ";") <$> l

findNode :: Text -> ModuleEnv -> Maybe NodeIdent
findNode nodeName sig =
  let n = NodeIdent . Ident $ nodeName
      vName = verilogName n
   in Map.foldrWithKey (find vName) Nothing sig
  where
    find :: NodeIdent -> ModuleName -> a -> Maybe NodeIdent -> Maybe NodeIdent
    find _ _ _ x@(Just _) = x
    find _ (Base _) _ Nothing = Nothing
    find _ MainModule _ Nothing = Nothing
    find toFind (Custom name) _ Nothing =
      if toFind == name
        then Just name
        else Nothing

toVerilogCode :: ResetKind -> Text -> Ast -> Maybe Text
toVerilogCode reset mainName (Ast sig modules imports) = toVerilogCode' <$> findNode mainName sig
  where
    toVerilogCode' :: NodeIdent -> Text
    toVerilogCode' mainModule =
      let imp = vsep $ ppImportDecl <$> imports
          main = buildMainModule reset sig mainModule
          fileModules = mapM (moduleToVerilog sig) modules
       in renderLazy . layoutPretty layoutOptions $
            (imp <> hardline)
              <> hardline
              <> concatModule (evalState ((:) <$> main <*> fileModules) 0)
              <> hardline

ppImportDecl :: ModuleImport -> Doc ann
ppImportDecl file = "`include" <+> angles (pretty file)

getModuleHead :: ModuleEnv -> ModuleName -> ModuleHead
getModuleHead env name = fromMaybe (error $ "Unable to find module " <> show name) $ Map.lookup name env

buildMainModule :: ResetKind -> ModuleEnv -> NodeIdent -> CallNumbering (Doc ann)
buildMainModule reset sigs mainName =
  let fst3 (x, _, _) = x
      snd3 (_, x, _) = x
      m = getModuleHead sigs (Custom mainName)
      (resetName, resetOp) = case reset of
        ActiveLow -> (Ident "reset_n", "!")
        ActiveHigh -> (Ident "reset", emptyDoc)
      initName = Ident "init"
      mControl = case controlVars m of
        Nothing -> Nothing
        Just ModuleControl {clockVar} ->
          let resetVar = WireDecl Unsigned (FixedSize (BVSize 1)) resetName
           in Just
                ( ModuleControl {clockVar, initVar = resetVar},
                  ModuleControl {clockVar = getVarName clockVar, initVar = initName},
                  getVarName clockVar
                )
      mainHead = ModuleHead MainModule (staticVars m) (fst3 <$> mControl) (inputVars m) (outputVars m)
      mainInst =
        ModuleInst
          { name = moduleName m,
            staticArgs = DeclaredValue . getStaticName <$> staticVars m,
            controlArgs = (snd3 <$> mControl),
            inArgs = getVarName <$> inputVars m,
            outArgs = getVarName <$> outputVars m
          }
      resetBody = case mControl of
        Nothing -> emptyDoc
        Just (_, _, clock) ->
          ("reg unsigned" <+> pretty initName <> ";" <> hardline)
            <> hardline
            <> ("initial begin" <> hardline)
            <> indent 2 (pretty initName <+> equals <+> pretty one <> ";")
            <> (hardline <> "end" <> hardline <> hardline)
            <> ("always @(posedge" <+> pretty clock <> ") begin" <> hardline)
            <> indent
              2
              ( ("if" <+> parens (resetOp <> pretty resetName) <+> pretty initName <+> "<=" <+> pretty one <> semi)
                  <> hardline
                  <> ("else" <+> pretty initName <+> "<=" <+> pretty zero <> semi)
              )
            <> (hardline <> "end" <> hardline <> hardline)
   in do
        inst <- moduleInst m mainInst
        return $
          (moduleHeadToVerilog mainHead <> hardline)
            <> indent bodyIdent (resetBody <> inst <> semi)
            <> (hardline <> "endmodule")

data VarKind = InputVar | OutputVar | InternalVar

ppVarDecl :: VarKind -> VarDecl -> Doc ann
ppVarDecl kind decl =
  let vDef = pretty decl
   in case kind of
        InputVar -> "input" <+> vDef
        OutputVar -> "output" <+> vDef
        InternalVar -> vDef

ppStaticDecl :: StaticDecl -> Doc ann
ppStaticDecl (StaticDecl name Nothing) = "parameter" <+> pretty name
ppStaticDecl (StaticDecl name (Just v)) = "parameter" <+> pretty name <+> equals <+> pretty v

moduleToVerilog :: ModuleEnv -> Module -> CallNumbering (Doc ann)
moduleToVerilog sigs m = node <$> moduleBodyToVerilog sigs (moduleBody m)
  where
    node body =
      (moduleHeadToVerilog (moduleHead m) <> hardline)
        <> indent bodyIdent (nodeBody body)
        <> (hardline <> "endmodule")

    nodeBody body = moduleLocalToVerilog (moduleLocal m) <> body

moduleHeadToVerilog :: ModuleHead -> Doc ann
moduleHeadToVerilog m =
  ("module" <+> pretty (moduleName m) <+> static <> "(" <> hardline)
    <> indent declIdent nodeVarDecl
    <> (hardline <> ");")
  where
    static = case nodeStaticDecl of
      [] -> emptyDoc
      l ->
        ("#(" <> hardline)
          <> indent declIdent (vsep l)
          <> (hardline <> ")" <> space)

    nodeStaticDecl = punctuate "," $ ppStaticDecl <$> staticVars m

    nodeVarDecl = vsep . punctuate "," $ nodeControlDecl <> nodeInputsDecl <> toList nodeOutputDecl
    nodeInputsDecl = ppVarDecl InputVar <$> inputVars m
    nodeOutputDecl = ppVarDecl OutputVar <$> outputVars m
    nodeControlDecl = ppVarDecl InputVar <$> maybe [] (\s -> [clockVar s, initVar s]) (controlVars m)

moduleLocalToVerilog :: [VarDecl] -> Doc ann
moduleLocalToVerilog [] = emptyDoc
moduleLocalToVerilog l = concatExpr (ppVarDecl InternalVar <$> l) <> hardline <> hardline

moduleBodyToVerilog :: ModuleEnv -> NonEmpty Expr -> CallNumbering (Doc ann)
moduleBodyToVerilog sigs eqs =
  let nodeExprList = mapM (exprToVerilog sigs) $ eqs
   in concatExpr . toList <$> nodeExprList

exprToVerilog :: ModuleEnv -> Expr -> CallNumbering (Doc ann)
exprToVerilog _ (VarExpr vDef val) =
  return $ "assign" <+> pretty vDef <+> "=" <+> pretty val
exprToVerilog _ (ConstExpr vDef val) =
  return $ "assign" <+> pretty vDef <+> "=" <+> pretty val
exprToVerilog sigs (InstExpr inst) =
  let m = getModuleHead sigs $ name inst in moduleInst m inst

moduleInst :: ModuleHead -> ModuleInst -> CallNumbering (Doc ann)
moduleInst m ModuleInst {name, staticArgs, controlArgs, inArgs, outArgs} =
  let callName instId = "call_" <> pretty name <> "_" <> pretty instId <+> "("
      callStatic = zipWith ppStaticArg (staticVars m) staticArgs
      callControlArgs = ppControlArgs (controlVars m) controlArgs
      callInArgs = zipWith ppVarArg (inputVars m) $ pretty <$> inArgs
      callOutArgs = NonEmpty.zipWith ppVarArg (outputVars m) $ pretty <$> outArgs
      callArgs = punctuate "," $ callControlArgs <> callInArgs <> toList callOutArgs
      static = case callStatic of
        [] -> emptyDoc
        l -> "#(" <> hardline <> indent 4 (vsep $ punctuate "," l) <> hardline <> ")" <> space
   in do
        iId <- freshId
        return $
          (pretty name <+> static <> callName iId <> hardline)
            <> indent 4 (vsep callArgs)
            <> (hardline <> ")")
  where
    ppStaticArg :: StaticDecl -> StaticValue -> Doc ann
    ppStaticArg (StaticDecl n _) (FixedValue v) = ppArg n $ pretty v
    ppStaticArg (StaticDecl n _) (DeclaredValue v) = ppArg n $ pretty v

    ppControlArgs :: Maybe (ModuleControl VarDecl) -> Maybe (ModuleControl Ident) -> [Doc ann]
    ppControlArgs Nothing _ = []
    ppControlArgs (Just ctrlDecl) (Just ctrlArgs) =
      [ ppVarArg (clockVar ctrlDecl) (pretty $ clockVar ctrlArgs),
        ppVarArg (initVar ctrlDecl) (pretty $ initVar ctrlArgs)
      ]
    ppControlArgs _ _ = error "Ill constructed call."

    ppVarArg :: VarDecl -> Doc ann -> Doc ann
    ppVarArg argDecl argVal = ppArg (getVarName argDecl) argVal

    ppArg :: Ident -> Doc ann -> Doc ann
    ppArg varName argVal = "." <> pretty varName <> parens argVal