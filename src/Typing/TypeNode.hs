{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Typing.TypeNode (typeAst) where

import Commons.Ast
import Commons.Error (CanFail, collapse, collapseA, reportError)
import Commons.Ids (Ident, NodeIdent, VarId (..), VarIdent)
import Commons.Position (Pos (..))
import Commons.Size (SimpleSize)
import Commons.Tree (Tree (..))
import Commons.Types (AtomicTType (..))
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Parsing.Ast
import Typing.Ast (TAst, TBody, TEquation)
import Typing.ExprEnv (ExprEnv, findVariable, runExprEnv)
import Typing.Intervals
import Typing.NodeEnv (NodeEnv, registerNode, runNodeEnv)
import Typing.NodeVarEnv
import Typing.SizeEnv (SizeInfo, buildNodeSizeEnv, restrictToInterval)
import Typing.TypeExpr (checkExprType)
import Typing.TypeUnification (TypeCand)

typeAst :: PAst -> CanFail TAst
typeAst (PAst l) = runNodeEnv $ mapM_ typeNode l

typeNode :: PNode -> NodeEnv ()
typeNode node = do
  let tnodeSizeInfo = buildNodeSizeEnv (nodeName node) (nodeSize node) (nodeSizeConstraints node)
  let tnodeEnv = collapse $ runNodeVarEnv typeVarDecls <$> tnodeSizeInfo
  (nId, reg) <- registerNode (nodeName node) (fst <$> tnodeEnv)
  nBody <- collapseA $ typeBody (nodeBody node) <$> nId <*> tnodeSizeInfo <*> (snd <$> tnodeEnv)
  reg nBody
  where
    typeVarDecls :: NodeVarEnv ()
    typeVarDecls = do
      mapM_ (typeDecl addInputVariable) (nodeInputs node)
      mapM_ (typeDecl addOutputVariable) (nodeOutputs node)

typeDecl :: (Pos Ident -> CanFail AtomicTType -> NodeVarEnv ()) -> IdentDecl -> NodeVarEnv ()
typeDecl f (IdentDecl declIdent declType) = typeType declType >>= f declIdent

typeType :: Pos LustreType -> NodeVarEnv (CanFail AtomicTType)
typeType t = typeType' $ unwrap t
  where
    typeType' BoolType = return . pure $ TBool
    typeType' (BitVectorType k s) = fmap (TBitVector k) <$> toSizeEq s

typeBody :: PBody -> NodeIdent -> SizeInfo -> VarMapping -> NodeEnv (CanFail (Body TBody))
typeBody (PSimpleBody b) nId sInfo vMap = fmap SimpleBody <$> typeNodeBody nId sInfo vMap b
typeBody PComposedBody {whenCriterion, whenBody, otherwiseBody, otherwiseLoc} nId sInfo vMap = do
  let intRes = normalizeInterval sInfo whenCriterion
  collapseA $ go <$> intRes
  where
    go :: IntervalResult -> NodeEnv (CanFail (Body TBody))
    go IntervalResult {crit, interval, holes} = do
      whenTbody <- toBodyWith crit whenBody interval
      otherwiseTBody <- catMaybes <$> mapM (toBodyWith crit otherwiseBody) holes
      return $ do
        whenTB <- case whenTbody of
          Nothing -> reportError whenCriterion "The size constraints for this section are not feasible in the node context."
          Just whenTB -> whenTB
        otherwiseTLi <- case otherwiseTBody of
          [] -> reportError otherwiseLoc "The size constraints for this section are not feasible in the node context."
          l -> sequenceA l
        return $ ComposedBody crit $ whenTB : otherwiseTLi

    toBodyWith :: SimpleSize -> PNodeBody -> Interval -> NodeEnv (Maybe (CanFail (Interval, TBody)))
    toBodyWith crit b it = sequenceA $ do
      int <- restrictToInterval sInfo crit it
      return $ fmap (it,) <$> typeNodeBody nId int vMap b

typeNodeBody :: NodeIdent -> SizeInfo -> VarMapping -> PNodeBody -> NodeEnv (CanFail TBody)
typeNodeBody nId sInfo vMap PNodeBody {pbodyLocals, pbodyEqs} = do
  let newVMap = fmap traceShowId $ addLocals sInfo vMap $ mapM_ (typeDecl addLocalVariable) pbodyLocals
  let tnodeEqs = fmap join . sequenceA <$> mapM typeEq pbodyEqs
  normEqs <- collapseA $ runExprEnv tnodeEqs nId <$> newVMap <*> pure sInfo
  return $ do
    nodeVMap <- newVMap
    (locaVMap, locNormEq) <- normEqs
    return $ NodeBody (Map.mapKeysMonotonic FromIdent nodeVMap <> locaVMap) locNormEq

typeEq :: Equation -> ExprEnv (CanFail (NonEmpty (TEquation TypeCand)))
typeEq (Equation p e) = do
  tPat <- typePat p
  collapseA $ flip checkExprType e <$> tPat

typePat :: Pattern -> ExprEnv (CanFail (Tree (VarIdent, AtomicTType)))
typePat (TreeLeaf x) = fmap TreeLeaf <$> findVariable x
typePat (TreeNode l) = fmap TreeNode . sequenceA <$> mapM typePat l
