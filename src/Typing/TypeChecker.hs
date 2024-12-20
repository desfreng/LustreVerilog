{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.TypeChecker (typeAst) where

import Commons.Ast
import Commons.BiList (BiList)
import qualified Commons.BiList as BiList
import Commons.Localized
import Commons.Tree (Tree (TreeLeaf, TreeNode))
import Commons.Types
import Commons.TypingError
import qualified Control.Monad as Monad
import Data.Bifunctor
import Data.Functor
import Parsing.Ast
import Typing.Ast
import Typing.AtomicUnifier (AtomicCand)
import Typing.Environments

typeAst :: Ast -> CanFail TAst
typeAst (Ast l) = runNodeEnv $ mapM_ typeNode l

typeNode :: Node -> NodeEnv ()
typeNode node = do
  nodeEnv <- runNodeVarEnv typeVarDecls
  tEqs <- sequenceA <$> mapM (typeCheckIn nodeEnv . typeEq) (nodeEqs node)
  addNode (nodeName node) nodeEnv tEqs
  where
    typeVarDecls = do
      mapM_ (typeDecl addInputVariable) (nodeInputs node)
      mapM_ (typeDecl addOutputVariable) (nodeOutputs node)
      mapM_ (typeDecl addLocalVariable) (nodeLocals node)

typeDecl :: (Localized Ident -> CanFail AtomicTType -> NodeVarEnv ()) -> IdentDecl -> NodeVarEnv ()
typeDecl f (IdentDecl declIdent declType) = typeType declType >>= f declIdent

typeType :: Localized LustreType -> NodeVarEnv (CanFail AtomicTType)
typeType t = typeType' $ unwrap t
  where
    typeType' BoolType = return . pure $ TBool
    typeType' (BitVectorType k s) = return . pure $ TBitVector k s

typeEq :: Equation -> ExprEnv (CanFail (TEquation TypeCand AtomicCand))
typeEq (Equation p e) = typePat p >>= collapseA . fmap buildEq
  where
    buildEq :: (TPattern, TType) -> ExprEnv (CanFail (TEquation TypeCand AtomicCand))
    buildEq (tp, typ) = fmap (tp,) <$> checkExprType typ e

typePat :: Pattern -> ExprEnv (CanFail (TPattern, TType))
typePat (TreeLeaf x) = fmap (bimap TreeLeaf TreeLeaf) <$> findVariable x
typePat (TreeNode l) = fmap (bimap TreeNode TreeNode . BiList.unzip) . sequenceA <$> mapM typePat l

type ExprCand = TExpr TypeCand AtomicCand

type ExprKindCand = TExprKind TypeCand AtomicCand

expectAtom :: ExpectedAtom -> Expr -> ExprEnv (CanFail (ExprCand, AtomicCand))
expectAtom expA e = findExprType e >>= collapseA . fmap expectAtom'
  where
    expectAtom' tCand = (exprType (unwrap tCand) >>= checkExpected (reportError e) expA) <&> fmap (tCand,)

checkExprType :: TType -> Expr -> ExprEnv (CanFail ExprCand)
checkExprType = typeExpr . Just

findExprType :: Expr -> ExprEnv (CanFail ExprCand)
findExprType = typeExpr Nothing

typeExpr :: Maybe TType -> Expr -> ExprEnv (CanFail ExprCand)
typeExpr typ e = fmap (e $>) <$> typeExpr' (unwrap e)
  where
    r = reportError e
    typeExpr' (ConstantExpr c) = typeConstantExpr r typ c
    typeExpr' (IdentExpr i) = typeIdentExpr r typ i
    typeExpr' (UnOpExpr op arg) = typeUnOpExpr r typ op arg
    typeExpr' (BinOpExpr op lhs rhs) = typeBinOpExpr r typ op lhs rhs
    typeExpr' (AppExpr n args) = typeAppExpr r typ n args
    typeExpr' (TupleExpr l) = typeTupleExpr r typ l
    typeExpr' (IfExpr cond tb fb) = typeIfExpr r typ cond tb fb
    typeExpr' (FbyExpr arg arg') = typeFbyExpr r typ arg arg'

buildAndUnify :: ErrorReporter -> Maybe TType -> ExprKindCand -> ExprEnv (CanFail ExprKindCand)
buildAndUnify err (Just typ) e = (exprType e >>= unifyWithTType err typ) <&> ($> e)
buildAndUnify _ Nothing e = return $ pure e

unifyExprs :: ErrorReporter -> ExprCand -> ExprCand -> ExprEnv (CanFail TypeCand)
unifyExprs err e1 e2 = do
  t1 <- exprType (unwrap e1)
  t2 <- exprType (unwrap e2)
  unifyTypeCand err t1 t2

typeConstantExpr :: ErrorReporter -> Maybe TType -> Constant -> ExprEnv (CanFail ExprKindCand)
typeConstantExpr err mtyp cst = constantTypeCand cst >>= buildAndUnify err mtyp . ConstantTExpr cst

typeIdentExpr :: ErrorReporter -> Maybe TType -> Localized Ident -> ExprEnv (CanFail ExprKindCand)
typeIdentExpr err mtyp var = findVariable var >>= collapseA . fmap buildIdent
  where
    buildIdent (varId, varTyp) = fromAtomicType varTyp >>= buildAndUnify err mtyp . VarTExpr varId

typeUnOpExpr :: ErrorReporter -> Maybe TType -> UnOp -> Expr -> ExprEnv (CanFail ExprKindCand)
typeUnOpExpr err mtyp op arg =
  case op of
    UnNot -> expectAtom expectBool arg >>= collapseA . fmap buildNot
    UnNeg -> expectAtom (expectBitVector Signed) arg >>= collapseA . fmap buildNeg
  where
    buildNot (targ, aT) = buildAndUnify err mtyp (UnOpTExpr UnNot targ aT)
    buildNeg (targ, aT) = buildAndUnify err mtyp (UnOpTExpr UnNeg targ aT)

typeBinOpExpr :: ErrorReporter -> Maybe TType -> BinOp -> Expr -> Expr -> ExprEnv (CanFail ExprKindCand)
typeBinOpExpr err mtyp op lhs rhs =
  case op of
    BinEq -> intEqOp
    BinNeq -> intEqOp
    BinLt -> intCmpOp
    BinLe -> intCmpOp
    BinGt -> intCmpOp
    BinGe -> intCmpOp
    BinAdd -> do
      tLhs <- expectAtom sigOrUnsig lhs
      tRhs <- expectAtom sigOrUnsig rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinSub -> do
      tLhs <- expectAtom (expectBitVector Signed) lhs
      tRhs <- expectAtom (expectBitVector Signed) rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinAnd -> boolOp
    BinOr -> boolOp
  where
    sigOrUnsig = expectBitVector Unsigned <> expectBitVector Signed
    rawSigOrUnsig = expectBitVector Raw <> expectBitVector Unsigned <> expectBitVector Signed

    intEqOp = do
      tLhs <- expectAtom rawSigOrUnsig lhs
      tRhs <- expectAtom rawSigOrUnsig rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    intCmpOp = do
      tLhs <- expectAtom sigOrUnsig lhs
      tRhs <- expectAtom sigOrUnsig rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    boolOp = do
      tLhs <- expectAtom expectBool lhs
      tRhs <- expectAtom expectBool rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    buildBoolOp (tLhs, lhsTyp) (tRhs, rhsTyp) =
      unifyAtomicCand err lhsTyp rhsTyp
        >>= embed . ($> fromAtomicType TBool)
        >>= collapseA . fmap (buildAndUnify err mtyp . BinOpTExpr op tLhs tRhs)

    buildBVOp (tLhs, lhsTyp) (tRhs, rhsTyp) =
      unifyAtomicCand err lhsTyp rhsTyp
        >>= collapseA . fmap (buildAndUnify err mtyp . BinOpTExpr op tLhs tRhs)

typeAppExpr ::
  ErrorReporter -> Maybe TType -> Localized Ident -> [Expr] -> ExprEnv (CanFail ExprKindCand)
typeAppExpr err mtyp node args = findNode node >>= collapseA . fmap typeAppExpr'
  where
    typeAppExpr' (nodeId, nodeSig) =
      let nodeNbArr = nodeArity nodeSig
          nbArgs = length args
          nodeIn = TreeLeaf <$> inputTypes nodeSig
       in case compare nodeNbArr nbArgs of
            LT ->
              err
                <$> "Too many arguments provided. Expected "
                  <> pure (show nodeNbArr)
                  <> ", found "
                  <> pure (show nbArgs)
                  <> "."
            GT ->
              err
                <$> "Too few arguments provided. Expected "
                  <> pure (show nodeNbArr)
                  <> ", found "
                  <> pure (show nbArgs)
                  <> "."
            EQ -> do
              tArgs <- sequenceA <$> Monad.zipWithM checkExprType nodeIn args
              outTyp <- nodeOutputTypeCand nodeSig
              collapseA $ buildAppExpr nodeId outTyp <$> tArgs

    buildAppExpr nodeId typ tArgs = buildAndUnify err mtyp (AppTExpr nodeId tArgs typ)

typeTupleExpr :: ErrorReporter -> Maybe TType -> BiList Expr -> ExprEnv (CanFail ExprKindCand)
typeTupleExpr err mtyp args = mapM findExprType args >>= collapseA . fmap buildTupleExpr . sequenceA
  where
    buildTupleExpr tArgs =
      mapM (exprType . unwrap) tArgs >>= tupleTypeCand >>= buildAndUnify err mtyp . TupleTExpr tArgs

typeIfExpr :: ErrorReporter -> Maybe TType -> Expr -> Expr -> Expr -> ExprEnv (CanFail ExprKindCand)
typeIfExpr err mtyp cond tb fb = do
  tCond <- expectAtom expectBool cond
  tTb <- findExprType tb
  tFb <- findExprType fb
  typ <- collapseA $ unifyExprs err <$> tTb <*> tFb
  collapseA $ buildIfExpr <$> tCond <*> tTb <*> tFb <*> typ
  where
    buildIfExpr (tCond, _) tTb tFb = buildAndUnify err mtyp . IfTExpr tCond tTb tFb

typeFbyExpr :: ErrorReporter -> Maybe TType -> Expr -> Expr -> ExprEnv (CanFail ExprKindCand)
typeFbyExpr err mtyp initE nextE = do
  tInitE <- findExprType initE
  tNextE <- findExprType nextE
  typ <- collapseA $ unifyExprs err <$> tInitE <*> tNextE
  collapseA $ buildFbyExpr <$> tInitE <*> tNextE <*> typ
  where
    buildFbyExpr tInitE tNextE = buildAndUnify err mtyp . FbyTExpr tInitE tNextE
