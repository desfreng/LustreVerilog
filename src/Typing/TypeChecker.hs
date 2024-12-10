{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.TypeChecker (module Typing.TypeChecker, module Typing.Ast) where

import Control.Monad
import Data.Bifunctor
import Data.Foldable ()
import Data.Functor
import Data.Text.Lazy (Text)
import Parsing.Ast
import Text.Megaparsec.Error (ParseErrorBundle)
import Typing.Ast
import Typing.Environments
import Typing.TypeError

typeAst :: FilePath -> Text -> Ast -> Either (ParseErrorBundle Text TypeError) TAst
typeAst path txt (Ast l) = runCanFail path txt $ runNodeEnv $ mapM_ typeNode l

typeNode :: Localized Node -> NodeEnv ()
typeNode locNode = do
  nodeEnv <- runNodeVarEnv typeVarDecls
  tEqs <- sequenceA <$> mapM (typeCheckIn nodeEnv . typeEq) (nodeEqs node)
  addNode locNode (nodeName node) nodeEnv tEqs
  where
    node = unwrap locNode

    typeVarDecls :: NodeVarEnv ()
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

typeEq :: Equation -> ExprEnv (CanFail (TEquation TypeCand))
typeEq (Equation p e) = typePat p >>= collapseA . fmap buildEq
  where
    buildEq :: (TPattern, TType) -> ExprEnv (CanFail (TEquation TypeCand))
    buildEq (tp, typ) = fmap (tp,) <$> checkExprType typ e

typePat :: Pattern -> ExprEnv (CanFail (TPattern, TType))
typePat (PatIdent x) = fmap (bimap TPatAtomic TAtom) <$> findVariable x
typePat (PatTuple l) = fmap (bimap TPatTuple TTuple . unbzip) . sequenceA <$> mapM typePat l

expectType :: ExpectedAtom -> Expr -> ExprEnv (CanFail (TExpr TypeCand))
expectType expAtom e = typeExpr Nothing e >>= collapseA . fmap (expectType' . unwrap)
  where
    expectType' (tE, tC) = fmap ((e $>) . (tE,)) <$> checkExpected (reportError e) expAtom tC

checkExprType :: TType -> Expr -> ExprEnv (CanFail (TExpr TypeCand))
checkExprType = typeExpr . Just

findExprType :: Expr -> ExprEnv (CanFail (TExpr TypeCand))
findExprType = typeExpr Nothing

typeExpr :: Maybe TType -> Expr -> ExprEnv (CanFail (TExpr TypeCand))
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

type TExprCand = TExprDesc TypeCand

buildAndUnify :: ErrorReporter -> Maybe TType -> TExprCand -> TypeCand -> ExprEnv (CanFail (TExprCand, TypeCand))
buildAndUnify err (Just typ) e t = fmap (e,) <$> unifyWithTType err typ t
buildAndUnify _ Nothing e t = return $ pure (e, t)

typeConstantExpr :: ErrorReporter -> Maybe TType -> Constant -> ExprEnv (CanFail (TExprCand, TypeCand))
typeConstantExpr err mtyp cst = constantTypeCand cst >>= buildAndUnify err mtyp (ConstantTExpr cst)

typeIdentExpr :: ErrorReporter -> Maybe TType -> Localized Ident -> ExprEnv (CanFail (TExprCand, TypeCand))
typeIdentExpr err mtyp var = findVariable var >>= collapseA . fmap buildIdent
  where
    buildIdent (varId, varTyp) = fromAtomicType varTyp >>= buildAndUnify err mtyp (VarTExpr varId)

typeUnOpExpr :: ErrorReporter -> Maybe TType -> UnOp -> Expr -> ExprEnv (CanFail (TExprCand, TypeCand))
typeUnOpExpr err mtyp op arg =
  case op of
    UnNot -> expectType expectBool arg >>= collapseA . fmap buildNot
    UnNeg -> expectType (expectBitVector Signed) arg >>= collapseA . fmap buildNeg
  where
    buildNot targ = buildAndUnify err mtyp (UnOpTExpr UnNot targ) (exprType targ)
    buildNeg targ = buildAndUnify err mtyp (UnOpTExpr UnNeg targ) (exprType targ)

typeBinOpExpr ::
  ErrorReporter -> Maybe TType -> BinOp -> Expr -> Expr -> ExprEnv (CanFail (TExprCand, TypeCand))
typeBinOpExpr err mtyp op lhs rhs =
  case op of
    BinEq -> intEqOp
    BinNeq -> intEqOp
    BinLt -> intCmpOp
    BinLe -> intCmpOp
    BinGt -> intCmpOp
    BinGe -> intCmpOp
    BinAdd -> do
      tLhs <- expectType sigOrUnsig lhs
      tRhs <- expectType sigOrUnsig rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinSub -> do
      tLhs <- expectType (expectBitVector Signed) lhs
      tRhs <- expectType (expectBitVector Signed) rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinAnd -> boolOp
    BinOr -> boolOp
  where
    sigOrUnsig = expectBitVector Unsigned <> expectBitVector Signed
    rawSigOrUnsig = expectBitVector Raw <> expectBitVector Unsigned <> expectBitVector Signed

    intEqOp = do
      tLhs <- expectType rawSigOrUnsig lhs
      tRhs <- expectType rawSigOrUnsig rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    intCmpOp = do
      tLhs <- expectType sigOrUnsig lhs
      tRhs <- expectType sigOrUnsig rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    boolOp = do
      tLhs <- expectType expectBool lhs
      tRhs <- expectType expectBool rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    buildBoolOp tLhs tRhs = do
      _ <- unifyTypeCand err (exprType tLhs) (exprType tRhs)
      resTyp <- fromAtomicType TBool
      buildAndUnify err mtyp (BinOpTExpr op tLhs tRhs) resTyp

    buildBVOp tLhs tRhs =
      unifyTypeCand err (exprType tLhs) (exprType tRhs)
        >>= collapseA . fmap (buildAndUnify err mtyp (BinOpTExpr op tLhs tRhs))

typeAppExpr ::
  ErrorReporter -> Maybe TType -> Localized Ident -> [Expr] -> ExprEnv (CanFail (TExprCand, TypeCand))
typeAppExpr err mtyp node args = findNode node >>= collapseA . fmap typeAppExpr'
  where
    typeAppExpr' (nodeId, nodeSig) =
      let nodeNbArr = nodeArity nodeSig
          nbArgs = length args
          nodeIn = TAtom <$> inputTypes nodeSig
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
              tArgs <- sequenceA <$> zipWithM checkExprType nodeIn args
              outTyp <- nodeOutputTypeCand nodeSig
              collapseA $ buildAppExpr nodeId outTyp <$> tArgs

    buildAppExpr nodeId typ tArgs = buildAndUnify err mtyp (AppTExpr nodeId tArgs) typ

typeTupleExpr :: ErrorReporter -> Maybe TType -> BiList Expr -> ExprEnv (CanFail (TExprCand, TypeCand))
typeTupleExpr err mtyp args = mapM findExprType args >>= collapseA . fmap buildTupleExpr . sequenceA
  where
    buildTupleExpr tArgs = tupleTypeCand (exprType <$> tArgs) >>= buildAndUnify err mtyp (TupleTExpr tArgs)

typeIfExpr :: ErrorReporter -> Maybe TType -> Expr -> Expr -> Expr -> ExprEnv (CanFail (TExprCand, TypeCand))
typeIfExpr err mtyp cond tb fb = do
  tCond <- expectType expectBool cond
  tTb <- findExprType tb
  tFb <- findExprType fb
  typ <- collapseA $ unifyBranches <$> tTb <*> tFb
  collapseA $ buildIfExpr <$> tCond <*> tTb <*> tFb <*> typ
  where
    unifyBranches tTb tFb = unifyTypeCand err (exprType tTb) (exprType tFb)
    buildIfExpr tCond tTb tFb = buildAndUnify err mtyp (IfTExpr tCond tTb tFb)

typeFbyExpr :: ErrorReporter -> Maybe TType -> Expr -> Expr -> ExprEnv (CanFail (TExprCand, TypeCand))
typeFbyExpr err mtyp initE nextE = do
  tInitE <- findExprType initE
  tNextE <- findExprType nextE
  typ <- collapseA $ unifyInitNext <$> tInitE <*> tNextE
  collapseA $ buildFbyExpr <$> tInitE <*> tNextE <*> typ
  where
    unifyInitNext tInitE tNextE = unifyTypeCand err (exprType tInitE) (exprType tNextE)
    buildFbyExpr tInitE tNextE = buildAndUnify err mtyp (FbyTExpr tInitE tNextE)
