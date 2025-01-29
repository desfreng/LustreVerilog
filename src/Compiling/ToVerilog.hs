{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiling.ToVerilog (toVerilogAst, verilogName) where

import Commons.Ast
import Commons.Ids
import Commons.Types
import Compiling.Ast
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as Text
import Verilog.Ast hiding (Constant)
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

encodeIdent :: CVar -> Ident
encodeIdent x = Ident $ "var_" <> Text.pack (show x)

toVerilogConstant :: CConstant -> Verilog.Constant
toVerilogConstant (CConstant _ (BoolConst True)) = Verilog.one
toVerilogConstant (CConstant _ (BoolConst False)) = Verilog.zero
toVerilogConstant (CConstant size (IntegerConst i)) = Verilog.Constant size i

encodeArgs :: CVal -> Either Verilog.Constant Ident
encodeArgs = bimap toVerilogConstant encodeIdent

getVarTyp :: Map CVar AtomicTType -> CVar -> AtomicTType
getVarTyp vTyp var = fromMaybe (error "Unable to retrieve variable typ.") (Map.lookup var vTyp)

getVarSize :: NodeContext CVar -> CVar -> BVSize
getVarSize NodeContext {nodeVarTypes} = typeSize . getVarTyp nodeVarTypes

getValSize :: NodeContext CVar -> CVal -> BVSize
getValSize _ (Left (CConstant size _)) = size
getValSize nCtx (Right var) = getVarSize nCtx var

varDecl :: NodeContext CVar -> CVar -> VarDecl
varDecl NodeContext {nodeVarTypes} var =
  let typ = getVarTyp nodeVarTypes var
   in case typ of
        TBool -> WireDecl (FixedSize $ BVSize 1) (encodeIdent var)
        TBitVector _ s -> WireDecl (FixedSize s) (encodeIdent var)
        TCustom _ _ -> error "Not Implemented"

ctrlDecl :: ModuleControl VarDecl
ctrlDecl =
  let decl var = WireDecl (FixedSize $ BVSize 1) $ Ident var
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
exprToVerilog _ _ (SimpleCEq v (SetValCAct val)) = AssignExpr (encodeIdent v) $ encodeArgs val
exprToVerilog ctrlArgs nCtx (SimpleCEq v (UnOpCAct op arg)) =
  let argSize = getVarSize nCtx v
      modArgs = [encodeArgs arg]
   in InstExpr $ ModuleInst (Base $ UnOpModule op) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (BinOpCAct op lhs rhs)) =
  let argSize = getValSize nCtx lhs
      modArgs = [encodeArgs lhs, encodeArgs rhs]
   in InstExpr $ ModuleInst (Base $ BinOpModule op) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (IfCAct cond tB fB)) =
  let argSize = getVarSize nCtx v
      modArgs = [Right . encodeIdent $ FromVarId cond, encodeArgs tB, encodeArgs fB]
   in InstExpr $ ModuleInst (Base IfModule) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (ConcatCAct fstArg sndArg)) =
  let staticArgs = [FixedValue $ getValSize nCtx fstArg, FixedValue $ getValSize nCtx sndArg]
      modArgs = [encodeArgs fstArg, encodeArgs sndArg]
   in InstExpr $ ModuleInst (Base ConcatModule) staticArgs ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (SliceCAct arg (fstIndex, sndIndex))) =
  let staticArgs = [FixedValue $ getValSize nCtx arg, FixedValue fstIndex, FixedValue sndIndex]
      modArgs = [encodeArgs arg]
   in InstExpr $ ModuleInst (Base SliceModule) staticArgs ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (SelectCAct arg index)) =
  let staticArgs = [FixedValue $ getValSize nCtx arg, FixedValue index]
      modArgs = [encodeArgs arg]
   in InstExpr $ ModuleInst (Base SelectModule) staticArgs ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs nCtx (SimpleCEq v (FbyCAct initVar nextVar)) =
  let argSize = getVarSize nCtx v
      modArgs = [encodeArgs initVar, encodeArgs nextVar]
   in InstExpr $ ModuleInst (Base FbyModule) [FixedValue argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs _ (CallCEq outArgs name inArgs) =
  let modArgs = encodeArgs <$> inArgs
   in InstExpr $ ModuleInst (Custom $ verilogName name) [] ctrlArgs modArgs (encodeIdent <$> outArgs)
