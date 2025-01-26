{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiling.ToVerilog (toVerilogAst, verilogName) where

import Commons.Ast
import Commons.Ids
import Commons.Types
import Compiling.Ast
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as Text
import Verilog.Ast hiding (Constant, Signed, Unsigned)
import qualified Verilog.Ast as Verilog
import Prelude hiding (init)

verilogName :: NodeIdent -> NodeIdent
verilogName (NodeIdent (Ident x)) = NodeIdent . Ident $ "node_" <> x

toVerilogAst :: CAst -> Verilog.Ast
toVerilogAst nodes = foldMap (uncurry go) nodes
  where
    go name node =
      let modName = verilogName name
          m = nodeToVerilog modName node
          sig = Map.singleton (Custom modName) $ moduleHead m
       in Verilog.Ast sig [m] []

encodeIdent :: (Show a) => a -> Ident
encodeIdent x = Ident $ "var_" <> Text.pack (show x)

getVarTyp :: Map CVar AtomicTType -> CVar -> AtomicTType
getVarTyp vTyp var = fromMaybe (error "Unable to retrieve variable typ.") (Map.lookup var vTyp)

getVarSize :: NodeContext CVar -> CVar -> BVSize
getVarSize NodeContext {nodeVarTypes} = typeSize . getVarTyp nodeVarTypes

varDecl :: NodeContext CVar -> CVar -> VarDecl
varDecl NodeContext {nodeVarTypes} var =
  let typ = getVarTyp nodeVarTypes var
   in case typ of
        TBool -> WireDecl Verilog.Unsigned (FixedSize $ BVSize 1) (encodeIdent var)
        TBitVector Raw s -> WireDecl Verilog.Unsigned (FixedSize s) (encodeIdent var)
        TBitVector Unsigned s -> WireDecl Verilog.Unsigned (FixedSize s) (encodeIdent var)
        TBitVector Signed s -> WireDecl Verilog.Signed (FixedSize s) (encodeIdent var)
        TCustom _ _ -> error "Not Implemented"

ctrlDecl :: ModuleControl VarDecl
ctrlDecl =
  let decl var = WireDecl Verilog.Unsigned (FixedSize $ BVSize 1) $ Ident var
   in ModuleControl (decl "clock") (decl "init")

nodeToVerilog :: NodeIdent -> CNode -> Module
nodeToVerilog moduleName (Node nCtx eqs) =
  let inputVars = varDecl nCtx . FromVarId . FromIdent <$> nodeInput nCtx
      outputVars = varDecl nCtx . FromVarId . FromIdent <$> nodeOutput nCtx
      moduleHead =
        ModuleHead
          { moduleName = Custom moduleName,
            staticVars = [],
            controlVars = Just ctrlDecl,
            inputVars,
            outputVars
          }
      moduleLocal = varDecl nCtx <$> toList (nodeLocal nCtx)
      moduleBody = exprToVerilog (Just $ toIdent ctrlDecl) nCtx <$> eqs
   in Module {moduleHead, moduleLocal, moduleBody}

exprToVerilog :: Maybe (ModuleControl Ident) -> NodeContext CVar -> CEquation -> Expr
exprToVerilog _ _ (SimpleCEq v (ConstantCAct cst)) =
  ConstExpr (encodeIdent v) (toVerilogConstant cst)
exprToVerilog _ _ (SimpleCEq v (VarCAct arg)) =
  VarExpr (encodeIdent v) (encodeIdent arg)
exprToVerilog ctrlArgs nCtx (SimpleCEq v (UnOpCAct op arg)) =
  let argSize = getVarSize nCtx v
      modArgs = [encodeIdent arg]
   in InstExpr $ ModuleInst (Base $ UnOpModule op) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (BinOpCAct op lhs rhs)) =
  let argSize = getVarSize nCtx v
      modArgs = [encodeIdent lhs, encodeIdent rhs]
   in InstExpr $ ModuleInst (Base $ BinOpModule op) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (IfCAct cond tB fB)) =
  let argSize = getVarSize nCtx v
      modArgs = [encodeIdent cond, encodeIdent tB, encodeIdent fB]
   in InstExpr $ ModuleInst (Base IfModule) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (FbyCAct initVar nextVar)) =
  let argSize = getVarSize nCtx v
      modArgs = [encodeIdent initVar, encodeIdent nextVar]
   in InstExpr $ ModuleInst (Base FbyModule) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs _ (CallCEq outArgs name inArgs) =
  let modArgs = (encodeIdent <$> inArgs)
   in InstExpr $ ModuleInst (Custom $ verilogName name) [] ctrlArgs modArgs (encodeIdent <$> outArgs)

toVerilogConstant :: CConstant -> Verilog.Constant
toVerilogConstant (CConstant _ (BoolConst True)) = Verilog.one
toVerilogConstant (CConstant _ (BoolConst False)) = Verilog.zero
toVerilogConstant (CConstant size (IntegerConst i)) = Verilog.Constant size i
