{-# LANGUAGE NamedFieldPuns #-}

module Compiling.Transform (transformAst) where

import Commons.Ast
import Commons.Ids
import Commons.Types
import Compiling.Ast
import Control.Monad.State
import Data.Bifunctor
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Typing.Ast

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

runTransformMonad :: NodeContext VarId -> TransformMonad (NonEmpty CEquation) -> CNode
runTransformMonad nCtx m =
  let (eqs, s) = runState m TransformState {newEqs = mempty, newVarsType = mempty, nextVarId = 0}
      newVars = Map.foldMapWithKey (\v _ -> Set.singleton v) $ newVarsType s
      newCtx = newContext nCtx FromVarId newVars $ newVarsType s
   in Node newCtx $ NonEmpty.prependList (newEqs s) eqs

transformAst :: TAst -> CAst
transformAst Ast {nodes} = second transformNode <$> nodes
  where
    transformNode (Node nCtx nEqs) = runTransformMonad nCtx $ mapM transformEq nEqs

transformEq :: TNodeEq -> TransformMonad CEquation
transformEq (SimpleTEq v arg) = SimpleCEq (FromVarId v) <$> transformExpr v arg
transformEq (FbyTEq v initE nextE) = do
  initVar <- defVar FbyInit v initE
  nextVar <- defVar FbyNext v nextE
  return $ SimpleCEq (FromVarId v) (FbyCAct initVar nextVar)
transformEq (CallTEq vars node args) =
  return $ CallCEq (FromVarId <$> vars) node (bimap buildCConst FromVarId <$> args)
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
isSignedOp _ = error "This type canot be signed."
