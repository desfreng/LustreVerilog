{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.CheckAtomicExpr
  ( expectAtomicType,
    typeConstantExpr,
    typeIdentExpr,
    typeUnOpExpr,
    typeBinOpExpr,
    typeConcatExpr,
    typeSelectExpr,
    typeSliceExpr,
    typeConvertExpr,
    checkCall,
  )
where

import Commons.Ast
import Commons.Ids
import Commons.Position
import Commons.Types
import Commons.TypingError
import qualified Control.Monad as Monad
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Parsing.Ast
import Typing.Ast
import Typing.ExprEnv
import Typing.TypeUnification

type AtomExprCand = (VarIdent, TExpr TypeCand, TypeCand)

getTypeCand :: AtomExprCand -> TypeCand
getTypeCand (_, _, x) = x

type TypeConstraint = (VarIdent, ExpectedType)

boolOrRaw :: VarIdent -> TypeConstraint
boolOrRaw var = (var, toAtom $ expectBool <> expectBitVector Raw)

sig :: VarIdent -> TypeConstraint
sig var = (var, toAtom $ expectBitVector Signed)

raw :: VarIdent -> TypeConstraint
raw var = (var, toAtom $ expectBitVector Raw)

sigOrUnsig :: VarIdent -> TypeConstraint
sigOrUnsig var = (var, toAtom $ expectBitVector Unsigned <> expectBitVector Signed)

rawSigOrUnsig :: VarIdent -> TypeConstraint
rawSigOrUnsig var = (var, toAtom $ expectBitVector Raw <> expectBitVector Unsigned <> expectBitVector Signed)

invalidTypeForExpr :: Pos a -> TypeConstraint -> CanFail b
invalidTypeForExpr loc eTyp = reportError loc $ "This expression does not have the type " <> show eTyp <> "."

expectAtomicType :: TypeConstraint -> Expr -> ExprEnv (CanFail AtomExprCand)
expectAtomicType typ e = typeAtomicExpr' (unwrap e)
  where
    typeAtomicExpr' (ConstantExpr c) = typeConstantExpr e typ c
    typeAtomicExpr' (IdentExpr i) = typeIdentExpr e typ i
    typeAtomicExpr' (UnOpExpr op arg) = typeUnOpExpr e typ op arg
    typeAtomicExpr' (BinOpExpr op lhs rhs) = typeBinOpExpr e typ op lhs rhs
    typeAtomicExpr' (IfExpr cond tb fb) = typeAtomicIfExpr e typ cond tb fb
    typeAtomicExpr' (AppExpr n args) = typeAtomicAppExpr e typ n args
    typeAtomicExpr' (TupleExpr _) = return $ invalidTypeForExpr e typ
    typeAtomicExpr' (FbyExpr arg arg') = typeAtomicFbyExpr e typ arg arg'
    typeAtomicExpr' (ConcatExpr lhs rhs) = typeConcatExpr e typ lhs rhs
    typeAtomicExpr' (SliceExpr arg i) = typeSliceExpr e typ arg i
    typeAtomicExpr' (SelectExpr arg i) = typeSelectExpr e typ arg i
    typeAtomicExpr' (ConvertExpr kind arg) = typeConvertExpr e typ kind arg

buildAndUnify :: Pos a -> TypeConstraint -> TExpr TypeCand -> ExprEnv (CanFail AtomExprCand)
buildAndUnify loc (var, typ) tExpr =
  unifToExpr (checkExpected loc typ (exprType tExpr)) <&> fmap (var,tExpr,)

typeConstantExpr :: Pos a -> TypeConstraint -> Constant -> ExprEnv (CanFail AtomExprCand)
typeConstantExpr loc typ cst =
  unifToExpr (constantTypeCand loc cst) >>= buildAndUnify loc typ . ConstantTExpr cst

typeIdentExpr :: Pos a -> TypeConstraint -> Pos Ident -> ExprEnv (CanFail AtomExprCand)
typeIdentExpr loc typ var = findVariable var >>= collapseA . fmap buildIdent
  where
    buildIdent (varId, varTyp) =
      unifToExpr (fromAtomicType loc varTyp)
        >>= buildAndUnify loc typ . VarTExpr (FromIdent varId)

typeUnOpExpr :: Pos a -> TypeConstraint -> UnOp -> Expr -> ExprEnv (CanFail AtomExprCand)
typeUnOpExpr loc typ@(var, _) op arg =
  case op of
    UnNot -> expectAtomicType (boolOrRaw var) arg >>= collapseA . fmap buildNot
    UnNeg -> expectAtomicType (sig var) arg >>= collapseA . fmap buildNeg
  where
    buildNot (_, targ, aT) = buildAndUnify loc typ (UnOpTExpr UnNot targ aT)
    buildNeg (_, targ, aT) = buildAndUnify loc typ (UnOpTExpr UnNeg targ aT)

typeBinOpExpr :: Pos a -> TypeConstraint -> BinOp -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeBinOpExpr loc typ@(var, _) op lhs rhs =
  case op of
    BinEq -> intEqOp
    BinNeq -> intEqOp
    BinLt -> intCmpOp
    BinLe -> intCmpOp
    BinGt -> intCmpOp
    BinGe -> intCmpOp
    BinAdd -> do
      tLhs <- expectAtomicType (sigOrUnsig var) lhs
      tRhs <- expectAtomicType (sigOrUnsig var) rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinSub -> do
      tLhs <- expectAtomicType (sig var) lhs
      tRhs <- expectAtomicType (sig var) rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinAnd -> boolOp
    BinOr -> boolOp
  where
    intEqOp = do
      tLhs <- expectAtomicType (rawSigOrUnsig var) lhs
      tRhs <- expectAtomicType (rawSigOrUnsig var) rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    intCmpOp = do
      tLhs <- expectAtomicType (sigOrUnsig var) lhs
      tRhs <- expectAtomicType (sigOrUnsig var) rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    boolOp = do
      tLhs <- expectAtomicType (boolOrRaw var) lhs
      tRhs <- expectAtomicType (boolOrRaw var) rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs

    buildBoolOp (_, tLhs, lhsTyp) (_, tRhs, rhsTyp) =
      unifToExpr (unifyTypeCand loc lhsTyp rhsTyp)
        >>= embed . ($> unifToExpr (fromAtomicType loc TBool))
        >>= collapseA . fmap (buildAndUnify loc typ . BinOpTExpr op tLhs tRhs)

    buildBVOp (_, tLhs, lhsTyp) (_, tRhs, rhsTyp) =
      unifToExpr (unifyTypeCand loc lhsTyp rhsTyp)
        >>= collapseA . fmap (buildAndUnify loc typ . BinOpTExpr op tLhs tRhs)

typeConcatExpr :: Pos a -> TypeConstraint -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeConcatExpr loc typ@(var, _) lhs rhs = do
  tLhs <- expectAtomicType (boolOrRaw var) lhs
  tRhs <- expectAtomicType (boolOrRaw var) rhs
  tLhsSize <- unifToExpr . collapseA $ getFixedSize lhs . getTypeCand <$> tLhs
  tRhsSize <- unifToExpr . collapseA $ getFixedSize rhs . getTypeCand <$> tRhs
  collapseA $ buildConcat <$> tLhsSize <*> tRhsSize <*> tLhs <*> tRhs
  where
    buildConcat (BVSize lhsSize) (BVSize rhsSize) (_, tLhs, _) (_, tRhs, _) =
      let size = BVSize $ lhsSize + rhsSize
       in unifToExpr (fromAtomicType loc . TBitVector Raw $ size)
            >>= buildAndUnify loc typ . ConcatTExpr tLhs tRhs

typeAtomicIfExpr :: Pos a -> TypeConstraint -> Expr -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeAtomicIfExpr loc typ@(var, _) cond tb fb = do
  tCond <- expectAtomicType (var, fromType TBool) cond
  condId <- embed $ buildIfCondEq <$> tCond
  tTb <- expectAtomicType typ tb
  tFb <- expectAtomicType typ fb
  collapseA $ buildIf <$> condId <*> tTb <*> tFb
  where
    buildIf condVarId (_, tTrue, typeTrue) (_, tFalse, typeFalse) =
      unifToExpr (unifyTypeCand loc typeTrue typeFalse)
        >>= collapseA . fmap (buildAndUnify loc typ . IfTExpr condVarId tTrue tFalse)

checkCall :: Pos a -> Pos Ident -> [Expr] -> ExprEnv (CanFail (NonEmpty (TExpr TypeCand)))
checkCall loc node args = findNode node >>= collapseA . fmap typeAppExpr'
  where
    typeAppExpr' n@(_, nodeSig) = checkInputs args nodeSig >>= embed . fmap (genOutVars n)
    snd3 (_, x, _) = x

    genOutVars nodeSig tArgs =
      let buildExpr (vId, vTyp) = do
            tc <- unifToExpr (fromAtomicType loc vTyp)
            return (VarTExpr vId tc)
       in buildCallEq nodeSig tArgs >>= mapM buildExpr

    checkInputs nodeArgs nodeSig =
      let nodeNbArr = nodeArity nodeSig
          nbArgs = length nodeArgs
       in case compare nodeNbArr nbArgs of
            LT ->
              return . reportError loc $
                "Too many arguments provided. Expected "
                  <> show nodeNbArr
                  <> ", found "
                  <> show nbArgs
                  <> "."
            GT ->
              return . reportError loc $
                "Too few arguments provided. Expected "
                  <> show nodeNbArr
                  <> ", found "
                  <> show nbArgs
                  <> "."
            EQ -> sequenceA <$> Monad.zipWithM unifyArg (inputTypes nodeSig) nodeArgs

    unifyArg (vName, atyp) expr = expectAtomicType (vName, fromType atyp) expr <&> fmap snd3

typeAtomicAppExpr :: Pos a -> TypeConstraint -> Pos Ident -> [Expr] -> ExprEnv (CanFail AtomExprCand)
typeAtomicAppExpr loc typ node args = checkCall loc node args >>= collapseA . fmap typeAtomicAppExpr'
  where
    typeAtomicAppExpr' (outVar :| []) = buildAndUnify loc typ outVar
    typeAtomicAppExpr' (_ :| _ : _) = return $ invalidTypeForExpr loc typ

typeAtomicFbyExpr :: Pos a -> TypeConstraint -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeAtomicFbyExpr loc typ initE nextE = do
  tInitE <- expectAtomicType typ initE
  tNextE <- expectAtomicType typ nextE
  collapseA $ buildFby <$> tInitE <*> tNextE
  where
    buildFby (v, tTrue, typeTrue) (_, tFalse, typeFalse) = do
      tCand <- unifToExpr (unifyTypeCand loc typeTrue typeFalse)
      fbyVar <- embed $ buildFbyEq v tTrue tFalse <$> tCand
      collapseA $ buildAndUnify loc typ <$> (VarTExpr <$> fbyVar <*> tCand)

typeSliceExpr :: Pos a -> TypeConstraint -> Expr -> (Int, Int) -> ExprEnv (CanFail AtomExprCand)
typeSliceExpr loc typ@(var, _) arg (i, j) = do
  tArg <- expectAtomicType (raw var) arg
  tArgSize <- unifToExpr . collapseA $ getFixedSize arg . getTypeCand <$> tArg
  collapseA $ buildSlice <$> tArgSize <*> tArg
  where
    buildSlice (BVSize busSize) (_, tArg, _) =
      if (0 <= i) && (i < j)
        then
          if j > busSize
            then return $ reportError loc $ "The slice window is too large. The argument has " <> show busSize <> " buses."
            else
              unifToExpr (fromAtomicType loc . TBitVector Raw . BVSize $ j - i)
                >>= buildAndUnify loc typ . SliceTExpr tArg (BVSize i, BVSize j)
        else return $ reportError loc $ "Ill formed slice."

typeSelectExpr :: Pos a -> TypeConstraint -> Expr -> Int -> ExprEnv (CanFail AtomExprCand)
typeSelectExpr loc typ@(var, _) arg index = do
  tArg <- expectAtomicType (raw var) arg
  tArgSize <- unifToExpr . collapseA $ getFixedSize arg . getTypeCand <$> tArg
  collapseA $ buildSelect <$> tArgSize <*> tArg
  where
    buildSelect (BVSize busSize) (_, tArg, _) =
      if (index < busSize) && (index >= 0)
        then
          unifToExpr (fromAtomicType loc TBool)
            >>= buildAndUnify loc typ . SelectTExpr tArg (BVSize index)
        else
          return $ reportError loc $ "Select bus does not exists. The argument has " <> show busSize <> " buses."

typeConvertExpr :: Pos a -> TypeConstraint -> BitVectorKind -> Expr -> ExprEnv (CanFail AtomExprCand)
typeConvertExpr loc typ@(var, _) kind arg = do
  tArg <- expectAtomicType (rawSigOrUnsig var) arg
  tArgSize <- unifToExpr . collapseA $ getFixedSize arg . getTypeCand <$> tArg
  collapseA $ buildConvert <$> tArgSize <*> tArg
  where
    buildConvert busSize (_, tArg, _) =
      unifToExpr (fromAtomicType loc (TBitVector kind busSize))
        >>= buildAndUnify loc typ . ConvertTExpr tArg
