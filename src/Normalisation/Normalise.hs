{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Normalisation.Normalise where

import Commons.Types (AtomicTType, TType)
import Commons.TypingError (CanFail)
import Data.List.NonEmpty (NonEmpty)
import Normalisation.Ast
import Typing.Ast

normalise :: TAst -> CanFail NAst
normalise TAst {tastNodeDecl, tastNodes} =
  NAst tastNodeDecl <$> traverse normaliseNode tastNodes
  where
    normaliseNode (nodeId, tnode) = (nodeId,) <$> nNode tnode

nNode :: TNode -> CanFail NNode
nNode TNode {tnodeCtx, tnodeEqs} = undefined tnodeCtx tnodeEqs

nEquation :: TEquation TType AtomicTType -> NonEmpty NEquation
nEquation = uncurry undefined

-- nExpr :: (NExpr -> a) -> TExpr TType -> a
-- nExpr f expr =
--   case unwrap expr of
--     (ConstantTExpr cst, TAtom typ) -> f (ConstantNExpr cst, typ)
--     (VarTExpr var, TAtom typ) -> f (VarNExpr (IsVarIdent var), typ)
--     (UnOpTExpr op e, TAtom typ) -> f (UnOpNExpr op (nAtomicExpr e), typ)
--     (BinOpTExpr op lhs rhs, TAtom typ) -> (BinOpNExpr op (nAtomicExpr lhs) (nAtomicExpr rhs), typ)
--     _ -> undefined