{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Verilog.Verilog (ResetKind (..), toVerilogCode) where

import Commons.Ast (Body (..), Bound (..), Interval (..))
import Commons.Ids (Ident (..), NodeIdent (..), SizeIdent (..))
import Commons.Size (SimpleSize, Size, constantSize, prettyRatio, varSize)
import Compiling.ToVerilog (verilogName)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (MonadState (state), State, evalState)
import Data.Foldable (Foldable (toList))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Debug.Trace (traceShow, traceShowId)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Text.Printf (printf)
import Verilog.Ast

bodyIdent :: Int
bodyIdent = 2

branchesIndent :: Int
branchesIndent = 2

declIdent :: Int
declIdent = 4

layoutOptions :: LayoutOptions
layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine 80 1.0}

data ResetKind = ActiveLow | ActiveHigh

newtype Output a = Output (ReaderT ModuleEnv (State Int) a)
  deriving (Functor, Applicative, Monad)

freshId :: Output Int
freshId = Output . state $ \s -> (s, s + 1)

getModuleHead :: ModuleName -> Output ModuleHead
getModuleHead name =
  Output . asks $ fromMaybe (error $ "Unable to find module " <> show name) . (Map.!? name)

runOutput :: ModuleEnv -> Output a -> a
runOutput sig (Output m) = evalState (runReaderT m sig) 0

findNode :: Text -> ModuleEnv -> Maybe NodeIdent
findNode nodeName env =
  let n = NodeIdent . Ident $ nodeName
      vName = traceShowId $ verilogName n
      nodeList = traceShowId $ mapMaybe go (Map.keys env)
   in if vName `elem` nodeList then Just n else Nothing
  where
    go :: ModuleName -> Maybe NodeIdent
    go (Base _) = Nothing
    go MainModule = Nothing
    go (Custom name) = Just name

concatModule :: [Doc ann] -> Doc ann
concatModule =
  let twoLines = hardline <> hardline
   in concatWith (\x y -> x <> twoLines <> y)

concatStmt :: [Doc ann] -> Doc ann
concatStmt l = vsep $ (<> ";") <$> l

toVerilogCode :: ResetKind -> Text -> Ast -> Either String Text
toVerilogCode reset mainName (Ast sig modules imports) =
  case findNode mainName sig of
    Nothing -> Left $ printf "There is no module %s" mainName
    Just m -> Right $ toVerilogCode' m
  where
    toVerilogCode' :: NodeIdent -> Text
    toVerilogCode' mainModule =
      let imp = vsep $ ppImportDecl <$> imports
          main = buildMainModule reset mainModule
          fileModules = mapM moduleToVerilog modules
          mods = runOutput sig $ (:) <$> main <*> fileModules
          doc = (imp <> hardline) <> hardline <> concatModule mods <> hardline
       in renderStrict $ layoutPretty layoutOptions doc

ppImportDecl :: ModuleImport -> Doc ann
ppImportDecl file = "`include" <+> angles (pretty file)

buildMainModule :: ResetKind -> NodeIdent -> Output (Doc ann)
buildMainModule reset mainName =
  let (resetName, resetOp) = case reset of
        ActiveLow -> (Ident "reset_n", "!")
        ActiveHigh -> (Ident "reset", emptyDoc)
      initName = Ident "init"
   in do
        m <- getModuleHead (Custom $ verilogName mainName)
        let mControl = case controlVars m of
              Nothing -> Nothing
              Just ModuleControl {clockVar} ->
                let resetVar = WireDecl (constantSize 1) resetName
                 in Just
                      ( ModuleControl {clockVar, initVar = resetVar},
                        ModuleControl {clockVar = getVarName clockVar, initVar = initName},
                        getVarName clockVar
                      )
        let mainHead = ModuleHead MainModule (moduleSize m) ((\(x, _, _) -> x) <$> mControl) (inputVars m) (outputVars m)
        let mainInst =
              ModuleInst
                { name = moduleName m,
                  sizeArgs = varSize <$> moduleSize m,
                  controlArgs = (\(_, x, _) -> x) <$> mControl,
                  inArgs = Right . getVarName <$> inputVars m,
                  outArgs = getVarName <$> outputVars m
                }
        let resetBody = case mControl of
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

ppStaticDecl :: SizeIdent -> Doc ann
ppStaticDecl name = "parameter" <+> pretty name

moduleToVerilog :: Module -> Output (Doc ann)
moduleToVerilog Module {moduleHead, moduleBody} = do
  body <- mapM sectionToVerilog moduleBody
  return $
    (moduleHeadToVerilog moduleHead <> hardline)
      <> indent bodyIdent (moduleBodyToVerilog body)
      <> (hardline <> "endmodule")
  where
    moduleBodyToVerilog :: Body (Doc ann) -> Doc ann
    moduleBodyToVerilog (SimpleBody body) = body
    moduleBodyToVerilog (ComposedBody {criterion, branches}) =
      concatWith (\x y -> x <> hardline <> y) $
        branchToVerilog criterion <$> branches

    branchToVerilog :: SimpleSize -> (Interval, Doc ann) -> Doc ann
    branchToVerilog crit (bound, d) =
      ("if" <+> parens (criterionToVerilog crit bound) <+> "begin" <> hardline)
        <> (indent branchesIndent d <> hardline)
        <> "end"

    criterionToVerilog :: SimpleSize -> Interval -> Doc ann
    criterionToVerilog s (MinoredBy (In i)) =
      prettyRatio i <+> "<=" <+> pretty s
    criterionToVerilog s (MinoredBy (Ex i)) =
      prettyRatio i <+> "<" <+> pretty s
    criterionToVerilog s (MajoredBy (In i)) =
      pretty s <+> "<=" <+> prettyRatio i
    criterionToVerilog s (MajoredBy (Ex i)) =
      pretty s <+> "<" <+> prettyRatio i
    criterionToVerilog s (Between (Ex lo) (Ex hi)) =
      prettyRatio lo <+> "<" <+> pretty s <+> "<" <+> prettyRatio hi
    criterionToVerilog s (Between (Ex lo) (In hi)) =
      prettyRatio lo <+> "<" <+> pretty s <+> "<=" <+> prettyRatio hi
    criterionToVerilog s (Between (In lo) (Ex hi)) =
      prettyRatio lo <+> "<=" <+> pretty s <+> "<" <+> prettyRatio hi
    criterionToVerilog s (Between (In lo) (In hi)) =
      if lo == hi
        then prettyRatio lo <+> "==" <+> pretty s
        else prettyRatio lo <+> "<=" <+> pretty s <+> "<=" <+> prettyRatio hi

moduleHeadToVerilog :: ModuleHead -> Doc ann
moduleHeadToVerilog m =
  ("module" <+> pretty (moduleName m) <+> static <> "(" <> hardline)
    <> indent declIdent nodeVarDecl
    <> (hardline <> ");")
  where
    static = case nodeSizeDecl of
      [] -> emptyDoc
      l ->
        ("#(" <> hardline)
          <> indent declIdent (vsep l)
          <> (hardline <> ")" <> space)

    nodeSizeDecl = punctuate "," $ ppStaticDecl <$> moduleSize m

    nodeVarDecl = vsep . punctuate "," $ nodeControlDecl <> nodeInputsDecl <> toList nodeOutputDecl
    nodeInputsDecl = ppVarDecl InputVar <$> inputVars m
    nodeOutputDecl = ppVarDecl OutputVar <$> outputVars m
    nodeControlDecl = ppVarDecl InputVar <$> maybe [] (\s -> [clockVar s, initVar s]) (controlVars m)

declListToVerilog :: [VarDecl] -> Doc ann
declListToVerilog [] = emptyDoc
declListToVerilog l = concatStmt (ppVarDecl InternalVar <$> l) <> hardline <> hardline

sectionToVerilog :: ModuleSection -> Output (Doc ann)
sectionToVerilog ModuleSection {sectionBody, sectionLocal} = do
  sectionExprList <- toList <$> mapM exprToVerilog sectionBody
  let sectionLocalList = declListToVerilog sectionLocal
  return $ sectionLocalList <> hardline <> concatStmt sectionExprList

valToVerilog :: Either Constant Ident -> Doc ann
valToVerilog = either pretty pretty

exprToVerilog :: Expr -> Output (Doc ann)
exprToVerilog (AssignExpr vDef val) =
  return $ "assign" <+> pretty vDef <+> "=" <+> valToVerilog val
exprToVerilog (InstExpr inst) = do
  m <- getModuleHead $ name inst
  moduleInst m inst

moduleInst :: ModuleHead -> ModuleInst -> Output (Doc ann)
moduleInst m ModuleInst {name, sizeArgs, controlArgs, inArgs, outArgs} =
  let callName instId = "call_" <> pretty name <> "_" <> pretty instId <+> "("
      callSize = zipWith ppStaticArg (moduleSize m) sizeArgs
      callControlArgs = ppControlArgs (controlVars m) controlArgs
      callInArgs = zipWith ppVarArg (inputVars m) $ valToVerilog <$> inArgs
      callOutArgs = NonEmpty.zipWith ppVarArg (outputVars m) $ pretty <$> outArgs
      callArgs = punctuate "," $ callControlArgs <> callInArgs <> toList callOutArgs
      static = case callSize of
        [] -> emptyDoc
        l -> "#(" <> hardline <> indent 4 (vsep $ punctuate "," l) <> hardline <> ")" <> space
   in do
        iId <- freshId
        return $
          (pretty name <+> static <> callName iId <> hardline)
            <> indent 4 (vsep callArgs)
            <> (hardline <> ")")
  where
    ppStaticArg :: SizeIdent -> Size -> Doc ann
    ppStaticArg (SizeIdent n) eq = ppArg n $ pretty eq

    ppControlArgs :: Maybe (ModuleControl VarDecl) -> Maybe (ModuleControl Ident) -> [Doc ann]
    ppControlArgs Nothing _ = []
    ppControlArgs (Just ctrlDecl) (Just ctrlArgs) =
      [ ppVarArg (clockVar ctrlDecl) (pretty $ clockVar ctrlArgs),
        ppVarArg (initVar ctrlDecl) (pretty $ initVar ctrlArgs)
      ]
    ppControlArgs _ _ = error "Ill constructed call."

    ppVarArg :: VarDecl -> Doc ann -> Doc ann
    ppVarArg argDecl = ppArg (getVarName argDecl)

    ppArg :: Ident -> Doc ann -> Doc ann
    ppArg varName argVal = "." <> pretty varName <> parens argVal
