{-# LANGUAGE RankNTypes #-}

module Typing.CheckAtomicExpr
  ( expectAtomicType,
    AtomExprCand (..),
    typeConstantExpr,
    typeIdentExpr,
    typeUnOpExpr,
    typeBinOpExpr,
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

newtype AtomExprCand = AtomExprCand {unAtomize :: TExprKind TypeCand}

invalidTypeForExpr :: Pos a -> ExpectedType -> CanFail b
invalidTypeForExpr loc eTyp = reportError loc $ "This expression does not have the type " <> show eTyp <> "."

expectAtomicType :: ExpectedType -> Expr -> ExprEnv (CanFail (TExpr TypeCand, TypeCand))
expectAtomicType typ e = fmap extractTypeCand <$> typeAtomicExpr' (unwrap e)
  where
    extractTypeCand (AtomExprCand tExpr) = (e $> tExpr, exprType tExpr)

    typeAtomicExpr' (ConstantExpr c) = typeConstantExpr e typ c
    typeAtomicExpr' (IdentExpr i) = typeIdentExpr e typ i
    typeAtomicExpr' (UnOpExpr op arg) = typeUnOpExpr e typ op arg
    typeAtomicExpr' (BinOpExpr op lhs rhs) = typeBinOpExpr e typ op lhs rhs
    typeAtomicExpr' (IfExpr cond tb fb) = typeAtomicIfExpr e typ cond tb fb
    typeAtomicExpr' (AppExpr n args) = typeAtomicAppExpr e typ n args
    typeAtomicExpr' (TupleExpr _) = return $ invalidTypeForExpr e typ
    typeAtomicExpr' (FbyExpr arg arg') = typeAtomicFbyExpr e typ arg arg'

buildAndUnify :: Pos a -> ExpectedType -> TExprKind TypeCand -> ExprEnv (CanFail AtomExprCand)
buildAndUnify loc typ tExpr = unifToExpr (checkExpected loc typ (exprType tExpr)) <&> ($> AtomExprCand (tExpr))

typeConstantExpr :: Pos a -> ExpectedType -> Constant -> ExprEnv (CanFail AtomExprCand)
typeConstantExpr loc typ cst =
  unifToExpr (constantTypeCand loc cst) >>= buildAndUnify loc typ . ConstantTExpr cst

typeIdentExpr :: Pos a -> ExpectedType -> Pos Ident -> ExprEnv (CanFail AtomExprCand)
typeIdentExpr loc typ var = findVariable var >>= collapseA . fmap buildIdent
  where
    buildIdent (varId, varTyp) = unifToExpr (fromAtomicType loc varTyp) >>= buildAndUnify loc typ . VarTExpr varId

typeUnOpExpr :: Pos a -> ExpectedType -> UnOp -> Expr -> ExprEnv (CanFail AtomExprCand)
typeUnOpExpr loc typ op arg =
  case op of
    UnNot -> expectAtomicType (toAtom expectBool) arg >>= collapseA . fmap buildNot
    UnNeg -> expectAtomicType (toAtom $ expectBitVector Signed) arg >>= collapseA . fmap buildNeg
  where
    buildNot (targ, aT) = buildAndUnify loc typ (UnOpTExpr UnNot targ aT)
    buildNeg (targ, aT) = buildAndUnify loc typ (UnOpTExpr UnNeg targ aT)

typeBinOpExpr :: Pos a -> ExpectedType -> BinOp -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeBinOpExpr loc typ op lhs rhs =
  case op of
    BinEq -> intEqOp
    BinNeq -> intEqOp
    BinLt -> intCmpOp
    BinLe -> intCmpOp
    BinGt -> intCmpOp
    BinGe -> intCmpOp
    BinAdd -> do
      tLhs <- expectAtomicType sigOrUnsig lhs
      tRhs <- expectAtomicType sigOrUnsig rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinSub -> do
      tLhs <- expectAtomicType sig lhs
      tRhs <- expectAtomicType sig rhs
      collapseA $ buildBVOp <$> tLhs <*> tRhs
    BinAnd -> boolOp
    BinOr -> boolOp
  where
    sig = toAtom $ expectBitVector Signed
    sigOrUnsig = toAtom $ expectBitVector Unsigned <> expectBitVector Signed
    rawSigOrUnsig = toAtom $ expectBitVector Raw <> expectBitVector Unsigned <> expectBitVector Signed

    intEqOp = do
      tLhs <- expectAtomicType rawSigOrUnsig lhs
      tRhs <- expectAtomicType rawSigOrUnsig rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    intCmpOp = do
      tLhs <- expectAtomicType sigOrUnsig lhs
      tRhs <- expectAtomicType sigOrUnsig rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    boolOp = do
      tLhs <- expectAtomicType (toAtom expectBool) lhs
      tRhs <- expectAtomicType (toAtom expectBool) rhs
      collapseA $ buildBoolOp <$> tLhs <*> tRhs

    buildBoolOp (tLhs, lhsTyp) (tRhs, rhsTyp) =
      unifToExpr (unifyTypeCand loc lhsTyp rhsTyp)
        >>= embed . ($> unifToExpr (fromAtomicType loc TBool))
        >>= collapseA . fmap (buildAndUnify loc typ . BinOpTExpr op tLhs tRhs)

    buildBVOp (tLhs, lhsTyp) (tRhs, rhsTyp) =
      unifToExpr (unifyTypeCand loc lhsTyp rhsTyp)
        >>= collapseA . fmap (buildAndUnify loc typ . BinOpTExpr op tLhs tRhs)

typeAtomicIfExpr :: Pos a -> ExpectedType -> Expr -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeAtomicIfExpr loc typ cond tb fb = do
  tCond <- expectAtomicType (fromType TBool) cond
  condId <- embed $ buildIfCondEq . fst <$> tCond
  tTb <- expectAtomicType typ tb
  tFb <- expectAtomicType typ fb
  collapseA $ buildIf <$> condId <*> tTb <*> tFb
  where
    buildIf condVarId (tTrue, typeTrue) (tFalse, typeFalse) =
      unifToExpr (unifyTypeCand loc typeTrue typeFalse)
        >>= collapseA . fmap (buildAndUnify loc typ . IfTExpr condVarId tTrue tFalse)

checkCall :: Pos a -> Pos Ident -> [Expr] -> ExprEnv (CanFail (NonEmpty (TExprKind TypeCand)))
checkCall loc node args = findNode node >>= collapseA . fmap typeAppExpr'
  where
    typeAppExpr' n@(_, nodeSig) = checkInputs args nodeSig >>= embed . fmap (genOutVars n)

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

    unifyArg atyp expr = expectAtomicType (fromType atyp) expr <&> fmap fst

typeAtomicAppExpr :: Pos a -> ExpectedType -> Pos Ident -> [Expr] -> ExprEnv (CanFail AtomExprCand)
typeAtomicAppExpr loc typ node args = checkCall loc node args >>= collapseA . fmap typeAtomicAppExpr'
  where
    typeAtomicAppExpr' (outVar :| []) = buildAndUnify loc typ outVar
    typeAtomicAppExpr' (_ :| _ : _) = return $ invalidTypeForExpr loc typ

typeAtomicFbyExpr :: Pos a -> ExpectedType -> Expr -> Expr -> ExprEnv (CanFail AtomExprCand)
typeAtomicFbyExpr loc typ initE nextE = do
  tInitE <- expectAtomicType typ initE
  tNextE <- expectAtomicType typ nextE
  collapseA $ buildFby <$> tInitE <*> tNextE
  where
    buildFby :: (TExpr TypeCand, TypeCand) -> (TExpr TypeCand, TypeCand) -> ExprEnv (CanFail AtomExprCand)
    buildFby (tTrue, typeTrue) (tFalse, typeFalse) = do
      tCand <- unifToExpr (unifyTypeCand loc typeTrue typeFalse)
      fbyVar <- embed $ buildFbyEq tTrue tFalse <$> tCand
      collapseA $ buildAndUnify loc typ <$> (VarTExpr <$> fbyVar <*> tCand)
