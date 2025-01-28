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

defVar :: (VarId -> Int -> CVar) -> VarId -> TExpr AtomicTType -> TransformMonad CVar
defVar gVar vId expr = transformExpr vId expr >>= defWithAct gVar vId (exprType expr)

defWithAct :: (VarId -> Int -> CVar) -> VarId -> AtomicTType -> CAction -> TransformMonad CVar
defWithAct gVar vId typ act = state $ freshVar' (gVar vId)
  where
    freshVar' genVar TransformState {newEqs, newVarsType, nextVarId} =
      let v = genVar nextVarId
          newNewEqs = SimpleCEq v act : newEqs
          newNewVarsType = Map.insert v typ newVarsType
          newNextId = nextVarId + 1
       in (v, TransformState newNewEqs newNewVarsType newNextId)

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
  return $ CallCEq (FromVarId <$> vars) node (FromVarId <$> args)

transformExpr :: VarId -> TExpr AtomicTType -> TransformMonad CAction
transformExpr _ (ConstantTExpr cst typ) = return $ ConstantCAct (CConstant (typeSize typ) cst)
transformExpr _ (VarTExpr vId _) = return $ VarCAct (FromVarId vId)
transformExpr v (UnOpTExpr op arg _) = defVar (UnOpArg op) v arg >>= transformUnOp op
transformExpr v (BinOpTExpr op lhs rhs typ) = do
  lVar <- defVar (BinOpArg op) v lhs
  rVar <- defVar (BinOpArg op) v rhs
  transformBinOp v typ op lVar rVar
transformExpr v (IfTExpr ifCond ifTrue ifFalse _) =
  IfCAct ifCond <$> defVar IfTrueBranch v ifTrue <*> defVar IfFalseBranch v ifFalse

transformUnOp :: UnOp -> CVar -> TransformMonad CAction
transformUnOp UnNeg arg = return $ UnOpCAct CUnNeg arg
transformUnOp UnNot arg = return $ UnOpCAct CUnNot arg

opKind :: AtomicTType -> CBinOp
opKind typ = if isSignedOp typ then CBinSignedLt else CBinUnsignedLt

transformBinOp :: VarId -> AtomicTType -> BinOp -> CVar -> CVar -> TransformMonad CAction
transformBinOp _ _ BinEq lhs rhs = return $ BinOpCAct CBinEq lhs rhs
transformBinOp v typ BinNeq lhs rhs =
  defWithAct (NotIntroduced BinNeq) v typ (BinOpCAct CBinEq lhs rhs) <&> UnOpCAct CUnNot
transformBinOp _ typ BinLt lhs rhs = return $ BinOpCAct (opKind typ) lhs rhs
transformBinOp v typ BinLe lhs rhs =
  -- lhs <= rhs <=> not(lhs > rhs) <=> not(rhs < lhs)
  defWithAct (NotIntroduced BinNeq) v typ (BinOpCAct (opKind typ) rhs lhs) <&> UnOpCAct CUnNot
transformBinOp _ typ BinGt lhs rhs =
  -- lhs > rhs <=> rhs < lhs
  return $ BinOpCAct (opKind typ) rhs lhs
transformBinOp v typ BinGe lhs rhs =
  -- lhs >= rhs <=> not(lhs < rhs)
  defWithAct (NotIntroduced BinNeq) v typ (BinOpCAct (opKind typ) lhs rhs) <&> UnOpCAct CUnNot
transformBinOp _ _ BinAdd lhs rhs = return $ BinOpCAct CBinAdd lhs rhs
transformBinOp _ _ BinSub lhs rhs = return $ BinOpCAct CBinSub lhs rhs
transformBinOp _ _ BinAnd lhs rhs = return $ BinOpCAct CBinAnd lhs rhs
transformBinOp _ _ BinOr lhs rhs = return $ BinOpCAct CBinOr lhs rhs

isSignedOp :: AtomicTType -> Bool
isSignedOp (TBitVector Signed _) = True
isSignedOp (TBitVector Unsigned _) = False
isSignedOp (TBitVector Raw _) = False
isSignedOp _ = error "This type canot be signed."
