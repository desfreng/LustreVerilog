{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiling.ToVerilog (toVerilogAst, verilogName) where

import Commons.Ast (Node (..), NodeBody (..))
import qualified Commons.Ast as Ast
import Commons.Ids (Ident (..), NodeIdent (..), VarId (..), VarIdent)
import Commons.Size (Size, constantSize)
import Commons.Types (AtomicTType, typeSize)
import Compiling.Ast (CAction (..), CAst, CBody, CConstant (..), CEquation (..), CNode, CVal, CVar (..))
import Control.Monad.Reader (Reader, ReaderT (..), asks, runReader)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Verilog.Ast

type SizeInformation = Map CVar Size

newtype VerilogSizer a = VerilogSizer {runSizer :: Reader SizeInformation a}
  deriving (Functor, Applicative, Monad)

getVarSize :: CVar -> VerilogSizer Size
getVarSize v =
  VerilogSizer $
    asks $ \m ->
      fromMaybe (error $ "Unable to retrieve variable type of " <> show v <> " from " <> show m) (m Map.!? v)

getValSize :: CVal -> VerilogSizer Size
getValSize (Left (CConstant size _)) = return size
getValSize (Right v) = getVarSize v

verilogName :: NodeIdent -> NodeIdent
verilogName (NodeIdent (Ident x)) = NodeIdent . Ident $ "node_" <> x

toVerilogAst :: CAst -> Ast
toVerilogAst = foldMap (uncurry go)
  where
    go name node =
      let modName = verilogName name
          m = nodeToVerilog modName node
          sig = Map.singleton (Custom modName) $ moduleHead m
       in Ast sig [m] []

encodeIdent :: CVar -> Ident
encodeIdent x = Ident $ "var_" <> Text.pack (show x)

toVerilogConstant :: CConstant -> Constant
toVerilogConstant (CConstant _ (Ast.BoolConst True)) = one
toVerilogConstant (CConstant _ (Ast.BoolConst False)) = zero
toVerilogConstant (CConstant size (Ast.IntegerConst i)) = Constant size i

encodeArgs :: CVal -> Either Constant Ident
encodeArgs = bimap toVerilogConstant encodeIdent

ctrlDecl :: ModuleControl VarDecl
ctrlDecl =
  let decl var = WireDecl (constantSize 1) $ Ident var
   in ModuleControl (decl "clock") (decl "init")

nodeToVerilog :: NodeIdent -> CNode -> Module
nodeToVerilog moduleName (Node nSig nBody) =
  let moduleHead =
        ModuleHead
          { moduleName = Custom moduleName,
            moduleSize = Ast.sizeVars nSig,
            controlVars = Just ctrlDecl,
            inputVars = topDecl <$> Ast.inputTypes nSig,
            outputVars = topDecl <$> Ast.outputTypes nSig
          }
      topDeclSize =
        typeSize
          <$> ( Map.fromList (Ast.inputTypes nSig)
                  <> Map.fromList (NonEmpty.toList $ Ast.outputTypes nSig)
              )
      moduleBody = bodyToVerilog topDeclSize <$> nBody
   in Module {moduleHead, moduleBody}
  where
    topDecl :: (VarIdent, AtomicTType) -> VarDecl
    topDecl = varDecl . first (FromVarId . FromIdent)

    varDecl :: (CVar, AtomicTType) -> VarDecl
    varDecl (vName, vTyp) = WireDecl (typeSize vTyp) (encodeIdent vName)

    bodyToVerilog :: Map VarIdent Size -> CBody -> ModuleSection
    bodyToVerilog vSize NodeBody {bodyLocal, bodyEqs} =
      let vMap = Map.mapKeysMonotonic (FromVarId . FromIdent) vSize <> (typeSize <$> bodyLocal)
          m = mapM (exprToVerilog (Just $ toIdent ctrlDecl)) bodyEqs
          sectionLocal = varDecl <$> Map.toList bodyLocal
          sectionBody = runReader (runSizer m) vMap
       in ModuleSection {sectionLocal, sectionBody}

exprToVerilog :: Maybe (ModuleControl Ident) -> CEquation -> VerilogSizer Expr
exprToVerilog _ (SimpleCEq v (SetValCAct val)) =
  return $ AssignExpr (encodeIdent v) $ encodeArgs val
exprToVerilog ctrlArgs (SimpleCEq v (UnOpCAct op arg)) = do
  argSize <- getVarSize v
  let modArgs = [encodeArgs arg]
  return $ InstExpr $ ModuleInst (Base $ UnOpModule op) [argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (SimpleCEq v (BinOpCAct op lhs rhs)) = do
  argSize <- getValSize lhs
  let modArgs = [encodeArgs lhs, encodeArgs rhs]
  return $ InstExpr $ ModuleInst (Base $ BinOpModule op) [argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (SimpleCEq v (IfCAct cond tB fB)) = do
  argSize <- getVarSize v
  let modArgs = [Right . encodeIdent $ FromVarId cond, encodeArgs tB, encodeArgs fB]
  return $ InstExpr $ ModuleInst (Base IfModule) [argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (SimpleCEq v (ConcatCAct fstArg sndArg)) = do
  staticArgs <- mapM getValSize [fstArg, sndArg]
  let modArgs = [encodeArgs fstArg, encodeArgs sndArg]
  return $ InstExpr $ ModuleInst (Base ConcatModule) staticArgs ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (SimpleCEq v (SliceCAct arg (fstIndex, sndIndex))) = do
  argSize <- getValSize arg
  let staticArgs = [argSize, fstIndex, sndIndex]
  let modArgs = [encodeArgs arg]
  return $ InstExpr $ ModuleInst (Base SliceModule) staticArgs ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (SimpleCEq v (SelectCAct arg index)) = do
  argSize <- getValSize arg
  let staticArgs = [argSize, index]
  let modArgs = [encodeArgs arg]
  return $ InstExpr $ ModuleInst (Base SelectModule) staticArgs ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (SimpleCEq v (FbyCAct initVar nextVar)) = do
  argSize <- getVarSize v
  let modArgs = [encodeArgs initVar, encodeArgs nextVar]
  return $ InstExpr $ ModuleInst (Base FbyModule) [argSize] ctrlArgs modArgs (encodeIdent v :| [])
exprToVerilog ctrlArgs (CallCEq outArgs name sizeArgs inArgs) =
  return $
    let modArgs = encodeArgs <$> inArgs
     in InstExpr $ ModuleInst (Custom $ verilogName name) sizeArgs ctrlArgs modArgs (encodeIdent <$> outArgs)
