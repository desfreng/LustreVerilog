{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.TypeExpr (checkExprType) where

import Commons.Ast
import Commons.BiList
import qualified Commons.BiList as BiList
import Commons.Ids
import Commons.Position
import Commons.Tree (Tree (..))
import qualified Commons.Tree as Tree
import Commons.Types
import Commons.TypingError
import Data.Bifunctor
import Data.Foldable1 (Foldable1 (head, toNonEmpty))
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Parsing.Ast
import Typing.Ast
import Typing.CheckAtomicExpr
import Typing.ExprEnv
import Typing.TypeUnification
import Prelude hiding (head)

type EqRHS = Tree (VarIdent, ExpectedType)

type ExprCand = Tree (VarIdent, TExpr TypeCand, TypeCand)

checkExprType :: Tree (VarIdent, AtomicTType) -> Expr -> ExprEnv (CanFail (NonEmpty (TEquation TypeCand)))
checkExprType rhs lhs =
  let rhsTyp = second fromType <$> rhs
   in typeExpr rhsTyp lhs <&> fmap buildEqs
  where
    buildEqs tEqs = toNonEmpty $ buildEq <$> tEqs
    buildEq (v, e, _) = SimpleTEq (FromIdent v) e

typeExpr :: EqRHS -> Expr -> ExprEnv (CanFail ExprCand)
typeExpr typ e = typeExpr' (unwrap e)
  where
    typeExpr' (ConstantExpr c) = cstExpr e typ c
    typeExpr' (IdentExpr i) = identExpr e typ i
    typeExpr' (UnOpExpr op arg) = unOpExpr e typ op arg
    typeExpr' (BinOpExpr op lhs rhs) = binOpExpr e typ op lhs rhs
    typeExpr' (IfExpr cond tb fb) = typeIfExpr e typ cond tb fb
    typeExpr' (AppExpr n args) = typeAppExpr e typ n args
    typeExpr' (TupleExpr l) = typeTupleExpr e typ l
    typeExpr' (FbyExpr arg arg') = typeFbyExpr e typ arg arg'
    typeExpr' (ConcatExpr lhs rhs) = concatExpr e typ lhs rhs
    typeExpr' (SliceExpr arg i) = sliceExpr e typ arg i
    typeExpr' (SelectExpr arg i) = selectExpr e typ arg i
    typeExpr' (ConvertExpr kind arg) = convertExpr e typ kind arg

invalidTypeForExpr :: Pos a -> EqRHS -> CanFail b
invalidTypeForExpr loc eTyp = reportError loc $ "This expression does not have the type " <> show eTyp <> "."

cstExpr :: Pos a -> EqRHS -> Constant -> ExprEnv (CanFail ExprCand)
cstExpr loc rhs@(TreeNode _) _ = return $ invalidTypeForExpr loc rhs
cstExpr loc (TreeLeaf typ) cst = fmap TreeLeaf <$> typeConstantExpr loc typ cst

identExpr :: Pos a -> EqRHS -> Pos Ident -> ExprEnv (CanFail ExprCand)
identExpr loc rhs@(TreeNode _) _ = return $ invalidTypeForExpr loc rhs
identExpr loc (TreeLeaf typ) vName = fmap TreeLeaf <$> typeIdentExpr loc typ vName

unOpExpr :: Pos a -> EqRHS -> UnOp -> Expr -> ExprEnv (CanFail ExprCand)
unOpExpr loc rhs@(TreeNode _) _ _ = return $ invalidTypeForExpr loc rhs
unOpExpr loc (TreeLeaf typ) op arg = fmap TreeLeaf <$> typeUnOpExpr loc typ op arg

binOpExpr :: Pos a -> EqRHS -> BinOp -> Expr -> Expr -> ExprEnv (CanFail ExprCand)
binOpExpr loc rhs@(TreeNode _) _ _ _ = return $ invalidTypeForExpr loc rhs
binOpExpr loc (TreeLeaf typ) op lhs rhs = fmap TreeLeaf <$> typeBinOpExpr loc typ op lhs rhs

concatExpr :: Pos a -> EqRHS -> Expr -> Expr -> ExprEnv (CanFail ExprCand)
concatExpr loc rhs@(TreeNode _) _ _ = return $ invalidTypeForExpr loc rhs
concatExpr loc (TreeLeaf typ) lhs rhs = fmap TreeLeaf <$> typeConcatExpr loc typ lhs rhs

sliceExpr :: Pos a -> EqRHS -> Expr -> (Int, Int) -> ExprEnv (CanFail ExprCand)
sliceExpr loc rhs@(TreeNode _) _ _ = return $ invalidTypeForExpr loc rhs
sliceExpr loc (TreeLeaf typ) arg index = fmap TreeLeaf <$> typeSliceExpr loc typ arg index

selectExpr :: Pos a -> EqRHS -> Expr -> Int -> ExprEnv (CanFail ExprCand)
selectExpr loc rhs@(TreeNode _) _ _ = return $ invalidTypeForExpr loc rhs
selectExpr loc (TreeLeaf typ) arg index = fmap TreeLeaf <$> typeSelectExpr loc typ arg index

convertExpr :: Pos a -> EqRHS -> BitVectorKind -> Expr -> ExprEnv (CanFail ExprCand)
convertExpr loc rhs@(TreeNode _) _ _ = return $ invalidTypeForExpr loc rhs
convertExpr loc (TreeLeaf typ) kind arg = fmap TreeLeaf <$> typeConvertExpr loc typ kind arg

buildAndUnify :: Pos a -> EqRHS -> Tree (TExpr TypeCand) -> ExprEnv (CanFail ExprCand)
buildAndUnify loc lhs rhs = do
  zipped <- Tree.zipWithM unifyLeaves lhs rhs
  case zipped of
    Just tExpr -> return $ sequenceA tExpr
    Nothing -> do
      rhsTyp <- Tree.showA showExpType lhs
      lhsTyp <- Tree.showA showTyp rhs
      return . reportError loc $ "Cannot unify " <> rhsTyp <> " with " <> lhsTyp <> "."
  where
    showTyp = unifToExpr . showTypeCand . exprType
    showExpType = return . show

    unifyLeaves (v, expE) tExpr = unifToExpr (checkExpected loc expE (exprType tExpr)) <&> fmap (v,tExpr,)

typeIfExpr :: Pos a -> EqRHS -> Expr -> Expr -> Expr -> ExprEnv (CanFail ExprCand)
typeIfExpr loc mtyp cond tb fb = do
  tCond <- expectAtomicType (fst $ head mtyp, fromType TBool) cond
  condId <- embed $ buildIfCondEq <$> tCond
  tTb <- typeExpr mtyp tb
  tFb <- typeExpr mtyp fb
  collapseA $ buildIfTree <$> condId <*> tTb <*> tFb
  where
    ter (_, _, x) = x

    buildIfTree condVarId tTb tFb = do
      zipped <- Tree.zipWithM (buildIf condVarId) tTb tFb
      case zipped of
        Nothing -> do
          lhsTyp <- Tree.showA (unifToExpr . showTypeCand . ter) tTb
          rhsTyp <- Tree.showA (unifToExpr . showTypeCand . ter) tFb
          return . reportError loc $ "Cannot unify type " <> lhsTyp <> " and " <> rhsTyp <> "."
        Just ifTree -> collapseA $ buildAndUnify loc mtyp <$> sequenceA ifTree

    buildIf condVarId (_, tTrue, trueTyp) (_, tFalse, falseTyp) = do
      tCand <- unifToExpr $ unifyTypeCand loc trueTyp falseTyp
      return $ IfTExpr condVarId tTrue tFalse <$> tCand

typeAppExpr :: Pos a -> EqRHS -> Pos Ident -> [Expr] -> ExprEnv (CanFail ExprCand)
typeAppExpr loc mtyp node args = checkCall loc node args >>= collapseA . fmap (typeAppExpr' . fmap TreeLeaf)
  where
    typeAppExpr' (outVar :| []) = buildAndUnify loc mtyp outVar
    typeAppExpr' (x :| y : l) = buildAndUnify loc mtyp (TreeNode (BiList x y l))

typeTupleExpr :: Pos a -> EqRHS -> BiList Expr -> ExprEnv (CanFail ExprCand)
typeTupleExpr loc mtyp args =
  case mtyp of
    TreeLeaf _ -> do
      eTyp <- Tree.showA (return . show) mtyp
      return . reportError loc $ "This expression can have the type " <> eTyp <> "."
    TreeNode typL -> typeTupleExpr' typL
  where
    numberMisMatch quant eL fL = do
      eTyp <- Tree.showA (return . show) mtyp
      return . reportError loc $
        "Too " <> quant <> " expressions provided. Expected " <> show eL <> " in " <> eTyp <> ", found " <> show fL <> "."

    snd3 (_, x, _) = x

    typeTupleExpr' mtypList =
      let typLength = length mtypList
          exprLength = length args
       in case compare typLength exprLength of
            LT -> numberMisMatch "many" typLength exprLength
            GT -> numberMisMatch "few" typLength exprLength
            EQ -> do
              tExpr <- BiList.zipWithM typeExpr mtypList args <&> buildTree
              collapseA $ buildAndUnify loc mtyp <$> tExpr

    buildTree Nothing = reportError loc "Unknown Typing Error (BiList should have the same size)"
    buildTree (Just tExprList) = TreeNode . fmap (fmap snd3) <$> sequenceA tExprList

typeFbyExpr :: Pos a -> EqRHS -> Expr -> Expr -> ExprEnv (CanFail ExprCand)
typeFbyExpr loc mtyp initE nextE = do
  initTExpr <- typeExpr mtyp initE
  nextTExpr <- typeExpr mtyp nextE
  collapseA $ buildFbyS <$> initTExpr <*> nextTExpr
  where
    ter (_, _, x) = x

    buildFbyS initTE nextTE = do
      zipped <- Tree.zipWithM buildFby initTE nextTE
      case zipped of
        Nothing -> do
          initTyp <- Tree.showA (unifToExpr . showTypeCand . ter) initTE
          nextTyp <- Tree.showA (unifToExpr . showTypeCand . ter) nextTE
          return . reportError loc $ "Cannot unify type " <> initTyp <> " and " <> nextTyp <> "."
        Just fbyTree -> collapseA $ buildAndUnify loc mtyp <$> sequenceA fbyTree

    buildFby (var, initExpr, initTyp) (_, nextExpr, nextTyp) = do
      tCand <- unifToExpr $ unifyTypeCand loc initTyp nextTyp
      fbyVar <- embed $ buildFbyEq var initExpr nextExpr <$> tCand
      return $ VarTExpr <$> fbyVar <*> tCand
