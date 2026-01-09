{-# LANGUAGE NamedFieldPuns #-}

module Compiling.Transform (transformAst) where

import Commons.Ast (Ast (..), BinOp (..), Node (..), NodeBody (..), UnOp (..))
import Commons.Ids (VarId)
import Commons.Types (AtomicTType (..), BitVectorKind (..), typeSize)
import Compiling.Ast (CAction (..), CAst, CBinOp (..), CBody, CConstant (..), CEquation (..), CNode, CUnOp (..), CVal, CVar (..))
import Control.Monad.State (MonadState (state), State, runState)
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Typing.Ast (TAst, TBody, TEquation (..), TExpr (..), TNode, TNodeEq, exprType)

data TransformState = TransformState
  { newEqs :: [CEquation],
    newVarsType :: Map CVar AtomicTType,
    nextVarId :: Int
  }

type TransformMonad = State TransformState

defVar :: (VarId -> Int -> CVar) -> VarId -> TExpr AtomicTType -> TransformMonad CVal
defVar gVar vId expr = transformExpr vId expr >>= defWithAct gVar vId (exprType expr)

defWithAct :: (VarId -> Int -> CVar) -> VarId -> AtomicTType -> CAction -> TransformMonad CVal
defWithAct _ _ _ (SetValCAct val) = return val
defWithAct gVar vId typ act = state $ freshVar' (gVar vId)
  where
    freshVar' genVar TransformState {newEqs, newVarsType, nextVarId} =
      let v = genVar nextVarId
          newNewEqs = SimpleCEq v act : newEqs
          newNewVarsType = Map.insert v typ newVarsType
          newNextId = nextVarId + 1
       in (Right v, TransformState newNewEqs newNewVarsType newNextId)

runTranformMonad :: TransformMonad (NonEmpty CEquation) -> (NonEmpty CEquation, TransformState)
runTranformMonad m =
  runState m TransformState {newEqs = mempty, newVarsType = mempty, nextVarId = 0}

transformAst :: TAst -> CAst
transformAst Ast {nodes} = second transformNode <$> nodes
  where
    transformNode :: TNode -> CNode
    transformNode (Node nSig nBody) = Node nSig $ transformBody <$> nBody

    transformBody :: TBody -> CBody
    transformBody NodeBody {bodyLocal, bodyEqs} =
      let (orgEq, s) = runTranformMonad $ mapM transformEq bodyEqs
          newLocals = Map.mapKeysMonotonic FromVarId bodyLocal <> newVarsType s
       in NodeBody newLocals $ NonEmpty.appendList orgEq $ newEqs s

transformEq :: TNodeEq -> TransformMonad CEquation
transformEq (SimpleTEq v arg) = SimpleCEq (FromVarId v) <$> transformExpr v arg
transformEq (FbyTEq v initE nextE) = do
  initVar <- defVar FbyInit v initE
  nextVar <- defVar FbyNext v nextE
  return $ SimpleCEq (FromVarId v) (FbyCAct initVar nextVar)
transformEq (CallTEq vars node sizeParams args) =
  return $ CallCEq (FromVarId <$> vars) node sizeParams (bimap buildCConst FromVarId <$> args)
  where
    buildCConst (cst, typ) = CConstant (typeSize typ) cst

transformExpr :: VarId -> TExpr AtomicTType -> TransformMonad CAction
transformExpr _ (ConstantTExpr cst typ) = return . SetValCAct . Left $ CConstant (typeSize typ) cst
transformExpr _ (VarTExpr vId _) = return . SetValCAct . Right $ FromVarId vId
transformExpr v (UnOpTExpr op arg _) = defVar (UnOpArg op) v arg >>= transformUnOp op
transformExpr v (BinOpTExpr op lhs rhs _) = do
  lVar <- defVar (BinOpArg op) v lhs
  rVar <- defVar (BinOpArg op) v rhs
  transformBinOp v op (exprType lhs, lVar) (exprType rhs, rVar)
transformExpr v (IfTExpr ifCond ifTrue ifFalse _) =
  IfCAct ifCond <$> defVar IfTrueBranch v ifTrue <*> defVar IfFalseBranch v ifFalse
transformExpr v (ConcatTExpr lhs rhs _) =
  ConcatCAct <$> defVar ConcatFirst v lhs <*> defVar ConcatSecond v rhs
transformExpr v (SliceTExpr arg index _) = SliceCAct <$> defVar SliceArg v arg <*> pure index
transformExpr v (SelectTExpr arg index _) = SelectCAct <$> defVar SelectArg v arg <*> pure index
transformExpr v (ConvertTExpr arg _) = SetValCAct <$> defVar ConvertArg v arg

transformUnOp :: UnOp -> CVal -> TransformMonad CAction
transformUnOp UnNeg arg = return $ UnOpCAct CUnNeg arg
transformUnOp UnNot arg = return $ UnOpCAct CUnNot arg

opKind :: AtomicTType -> AtomicTType -> CBinOp
opKind ltyp _ = if isSignedOp ltyp then CBinSignedLt else CBinUnsignedLt

transformBinOp :: VarId -> BinOp -> (AtomicTType, CVal) -> (AtomicTType, CVal) -> TransformMonad CAction
transformBinOp _ BinEq (_, lhs) (_, rhs) = return $ BinOpCAct CBinEq lhs rhs
transformBinOp v BinNeq (_, lhs) (_, rhs) =
  defWithAct (NotIntroduced BinNeq) v TBool (BinOpCAct CBinEq lhs rhs) <&> UnOpCAct CUnNot
transformBinOp _ BinLt (ltyp, lhs) (rtyp, rhs) = return $ BinOpCAct (opKind ltyp rtyp) lhs rhs
transformBinOp v BinLe (ltyp, lhs) (rtyp, rhs) =
  -- lhs <= rhs <=> not(lhs > rhs) <=> not(rhs < lhs)
  defWithAct (NotIntroduced BinNeq) v TBool (BinOpCAct (opKind ltyp rtyp) rhs lhs) <&> UnOpCAct CUnNot
transformBinOp _ BinGt (ltyp, lhs) (rtyp, rhs) =
  -- lhs > rhs <=> rhs < lhs
  return $ BinOpCAct (opKind ltyp rtyp) rhs lhs
transformBinOp v BinGe (ltyp, lhs) (rtyp, rhs) =
  -- lhs >= rhs <=> not(lhs < rhs)
  defWithAct (NotIntroduced BinNeq) v TBool (BinOpCAct (opKind ltyp rtyp) lhs rhs) <&> UnOpCAct CUnNot
transformBinOp _ BinAdd (_, lhs) (_, rhs) = return $ BinOpCAct CBinAdd lhs rhs
transformBinOp _ BinSub (_, lhs) (_, rhs) = return $ BinOpCAct CBinSub lhs rhs
transformBinOp _ BinAnd (_, lhs) (_, rhs) = return $ BinOpCAct CBinAnd lhs rhs
transformBinOp _ BinOr (_, lhs) (_, rhs) = return $ BinOpCAct CBinOr lhs rhs

isSignedOp :: AtomicTType -> Bool
isSignedOp (TBitVector Signed _) = True
isSignedOp (TBitVector Unsigned _) = False
isSignedOp (TBitVector Raw _) = False
isSignedOp _ = error "This type cannot be signed."
