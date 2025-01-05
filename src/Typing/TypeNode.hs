{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.TypeNode (typeAst) where

import Commons.Ids
import Commons.Position
import Commons.Tree (Tree (..))
import Commons.Types
import Commons.TypingError
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty)
import Parsing.Ast
import Typing.Ast
import Typing.ExprEnv
import Typing.NodeEnv
import Typing.NodeVarEnv
import Typing.TypeExpr (checkExprType)
import Typing.TypeUnification (TypeCand)

typeAst :: PAst -> CanFail TAst
typeAst (PAst l) = runNodeEnv $ mapM_ typeNode l

typeNode :: PNode -> NodeEnv ()
typeNode node =
  let tnodeEnv = runNodeVarEnv typeVarDecls
      tnodeEqs = fmap join . sequenceA <$> mapM typeEq (nodeEqs node)
   in runExprEnv tnodeEnv tnodeEqs >>= addNode (nodeName node)
  where
    typeVarDecls = do
      mapM_ (typeDecl addInputVariable) (nodeInputs node)
      mapM_ (typeDecl addOutputVariable) (nodeOutputs node)
      mapM_ (typeDecl addLocalVariable) (nodeLocals node)

typeDecl :: (Pos Ident -> CanFail AtomicTType -> NodeVarEnv ()) -> IdentDecl -> NodeVarEnv ()
typeDecl f (IdentDecl declIdent declType) = typeType declType >>= f declIdent

typeType :: Pos LustreType -> NodeVarEnv (CanFail AtomicTType)
typeType t = typeType' $ unwrap t
  where
    typeType' BoolType = return . pure $ TBool
    typeType' (BitVectorType k s) = return . pure $ TBitVector k s

typeEq :: Equation -> ExprEnv (CanFail (NonEmpty (TEquation TypeCand)))
typeEq (Equation p e) = do
  tPat <- typePat p
  collapseA $ (flip checkExprType) e <$> tPat

typePat :: Pattern -> ExprEnv (CanFail (Tree (VarIdent, AtomicTType)))
typePat (TreeLeaf x) = fmap TreeLeaf <$> findVariable x
typePat (TreeNode l) = fmap TreeNode . sequenceA <$> mapM typePat l
