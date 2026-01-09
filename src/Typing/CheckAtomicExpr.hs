{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

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

import Commons.Ast (BinOp (..), Constant, NodeSignature (..), SizeConstraint (..), UnOp (..))
import Commons.Error (CanFail, collapseA, embed, reportError)
import Commons.Ids (Ident, NodeIdent, SizeIdent, VarId (..), VarIdent)
import Commons.Position (Pos (..))
import Commons.Size (Size, hasVar, subSize, subst, sumSize, varSize)
import Commons.Types (AtomicTType (..), BitVectorKind (..))
import Control.Monad (zipWithM)
import Data.Bifunctor (second)
import Data.Functor (($>), (<&>))
import Data.List (intercalate, unsnoc)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Ap (..))
import Parsing.Ast (Expr, ExprDesc (..), SizeExpr)
import Text.Printf (printf)
import Typing.Ast (TExpr (..), exprType)
import Typing.ExprEnv
import Typing.TypeUnification

foldMapM :: (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
foldMapM f = getAp <$> foldMap (Ap . f)

zipWithM' :: (Monad m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM' f x = mapM (uncurry f) . NonEmpty.zip x

type AtomExprCand = (VarIdent, TExpr TypeCand, TypeCand)

getTypeCand :: AtomExprCand -> TypeCand
getTypeCand (_, _, x) = x

getTExpr :: AtomExprCand -> TExpr TypeCand
getTExpr (_, x, _) = x

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

buildArgConstr :: AtomicTType -> ExpectedType
buildArgConstr TBool = toAtom $ expectBool <> expectBitVector Raw
buildArgConstr (TBitVector k _) = toAtom $ expectBitVector k

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

typeIdentExpr :: Pos a -> TypeConstraint -> Ident -> ExprEnv (CanFail AtomExprCand)
typeIdentExpr loc typ var = findVariable (var <$ loc) >>= collapseA . fmap buildIdent
  where
    buildIdent (varId, varTyp) =
      unifToExpr (fromAtomicType loc "" varTyp)
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
        >>= embed . ($> unifToExpr (fromAtomicType loc "" TBool))
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
    buildConcat lhsSize rhsSize (_, tLhs, _) (_, tRhs, _) =
      let size = sumSize lhsSize rhsSize
       in unifToExpr (fromAtomicType loc "" . TBitVector Raw $ size)
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
checkCall loc node args = do
  nSig <- findNode node
  collapseA $ checkCall' loc args <$> nSig

checkCall' :: Pos a -> [Expr] -> (NodeIdent, NodeSignature) -> ExprEnv (CanFail (NonEmpty (TExpr TypeCand)))
checkCall' loc args (nodeName, nodeSig) = do
  inArgs <- checkInputsLen args
  sizeConstr <- embed $ fmap catMaybes . zipWithM gatherConstr (inputTypes nodeSig) . zip args <$> inArgs -- Gather Constraints
  sizeSol <- unifToExpr $ collapseA $ solveSystem loc nodeSig <$> sizeConstr -- Solve Constraints
  collapseA $ typeAppExpr' <$> inArgs <*> sizeSol
  where
    typeAppExpr' :: [AtomExprCand] -> SystemResult -> ExprEnv (CanFail (NonEmpty (TExpr TypeCand)))
    typeAppExpr' tArgs sizeSol = do
      tInArgs <- fmap sequenceA . sequenceA $ zipWith3 (unifyInputs sizeSol) args tArgs (inputTypes nodeSig) -- Forall non Constrained input, unify them
      sConstr <- foldMapM (checkConstraint sizeSol) (sizeConstraints nodeSig) -- Checks Callee size constraints
      recCall <- checkRecCall sizeSol -- Check Reccursive call
      embed $ do () <- (<>) <$> sConstr <*> recCall; genOutVars sizeSol <$> tInArgs

    genOutVars :: SystemResult -> [TExpr TypeCand] -> ExprEnv (NonEmpty (TExpr TypeCand))
    genOutVars sizeSol tArgs =
      let ctx = showContext nodeName sizeSol
          buildExpr (orgId, orgTyp) (vId, vTyp) = do
            tc <- unifToExpr (fromAtomicType loc (ctx orgId orgTyp) vTyp)
            return (VarTExpr vId tc)
          substType typ = case typ of
            TBool -> TBool
            TBitVector k s -> TBitVector k $ subst (sizeSol !) s
          sizeExpr = (sizeSol !) . fst <$> sizeVars nodeSig
          tArgsTyped = zip tArgs $ substType . snd <$> inputTypes nodeSig
          outputType = substType . snd <$> outputTypes nodeSig
       in do
            outVars <- buildCallEq nodeName sizeExpr tArgsTyped outputType
            zipWithM' buildExpr (outputTypes nodeSig) outVars

    checkInputsLen :: [Expr] -> ExprEnv (CanFail [AtomExprCand])
    checkInputsLen nodeArgs =
      let nodeNbArr = nodeArity nodeSig
          nbArgs = length nodeArgs
       in case compare nodeNbArr nbArgs of
            LT -> return $ reportError loc $ printf "Too many arguments provided. Expected %d, found %d." nodeNbArr nbArgs
            GT -> return $ reportError loc $ printf "Too few arguments provided. Expected %d, found %d." nodeNbArr nbArgs
            EQ -> sequenceA <$> zipWithM unifyArg nodeArgs (inputTypes nodeSig)

    checkConstraint :: SystemResult -> SizeConstraint Size -> ExprEnv (CanFail ())
    checkConstraint sizeSol cstr = do
      res <- unifToExpr $ checkSizeConstraint $ subst (sizeSol !) <$> cstr
      return $
        if res
          then pure ()
          else reportError loc $ printf "Unable to verify %s." (show $ subst (sizeSol !) <$> cstr)

    unifyArg :: Expr -> (VarIdent, AtomicTType) -> ExprEnv (CanFail AtomExprCand)
    unifyArg arg nodeArg = expectAtomicType (second buildArgConstr nodeArg) arg

    unifyInputs :: SystemResult -> Pos a -> AtomExprCand -> (VarIdent, AtomicTType) -> ExprEnv (CanFail (TExpr TypeCand))
    unifyInputs sizeSol argLoc (_, tArg, _) t =
      fmap getTExpr <$> buildAndUnify argLoc (buildArgType sizeSol t) tArg

    buildArgType :: SystemResult -> (VarIdent, AtomicTType) -> TypeConstraint
    buildArgType _ (argName, TBool) = (argName, fromType TBool)
    buildArgType sizeSol (argName, TBitVector k s) =
      (argName, fromType $ TBitVector k $ subst (sizeSol !) s)

    gatherConstr :: (VarIdent, AtomicTType) -> (Pos a, AtomExprCand) -> ExprEnv (Maybe (Pos (Size, Size)))
    gatherConstr (_, TBool) (_, (_, _, _)) = return Nothing
    gatherConstr (_, TBitVector _ s) (argLoc, (_, _, tc)) =
      do
        argSize <- unifToExpr $ getSize tc
        return $ case argSize of
          Left _ -> Nothing
          Right aS -> Just $ argLoc $> (s, aS)

    showResult :: (SizeIdent -> Bool) -> SystemResult -> String
    showResult p sizeSol =
      let showEq (s, sEq) = printf "%s = %s" (show s) (show sEq)
          varEq = showEq <$> Map.toList (Map.filterWithKey (\s _ -> p s) sizeSol)
       in case unsnoc varEq of
            Nothing -> ""
            Just ([], l) -> l
            Just (hds, l) -> printf "%s and %s" (intercalate ", " hds) l

    showContext :: NodeIdent -> SystemResult -> VarIdent -> AtomicTType -> String
    showContext nodeId sizeSol orgVarName orgVarType =
      let isInType v =
            case orgVarType of
              TBool -> False
              TBitVector _ s -> hasVar s v
       in printf
            "Instantiation of output variable %s (of type %s) from node %s with %s."
            (show orgVarName)
            (show orgVarType)
            (show nodeId)
            $ showResult isInType sizeSol

    checkRecCall :: SystemResult -> ExprEnv (CanFail ())
    checkRecCall sizeSol = do
      sameNode <- isCurrentNode nodeName
      if sameNode
        then do
          lexOrderOk <- checkLexOrder $ (\s -> (s, sizeSol ! s)) . fst <$> sizeVars nodeSig
          if not lexOrderOk
            then return $ reportError loc $ "This node instantiation is not stricly decreasing in size. We have " <> showResult (const True) sizeSol <> "."
            else return $ pure ()
        else return $ pure ()

    checkLexOrder :: [(SizeIdent, Size)] -> ExprEnv Bool
    checkLexOrder [] = return False
    checkLexOrder ((x, y) : rest) = do
      isLt <- unifToExpr $ y `isStrictlySmaller` varSize x
      isEq <- unifToExpr $ y `isEqual` varSize x
      if isLt
        then return True
        else
          if isEq
            then checkLexOrder rest
            else return False

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

typeSliceExpr :: Pos a -> TypeConstraint -> Expr -> (SizeExpr, SizeExpr) -> ExprEnv (CanFail AtomExprCand)
typeSliceExpr loc typ@(var, _) arg (begSizeExpr, endSizeExpr) = do
  tArg <- expectAtomicType (raw var) arg
  tArgSize <- unifToExpr . collapseA $ getFixedSize arg . getTypeCand <$> tArg
  begSize <- unifToExpr $ checkSizeExpr begSizeExpr -- begSize >= 0 is implicit
  endSize <- unifToExpr $ checkSizeExpr endSizeExpr -- endSize >= 0 is implicit
  collapseA $ buildSlice <$> tArgSize <*> tArg <*> begSize <*> endSize
  where
    buildSlice :: Size -> AtomExprCand -> Size -> Size -> ExprEnv (CanFail AtomExprCand)
    buildSlice busSize (_, tArg, _) i j = do
      isInBound <- unifToExpr $ j `isSmaller` busSize -- j <= busSize
      isWellFormed <- unifToExpr $ i `isStrictlySmaller` j -- i < j
      if isInBound
        then
          if isWellFormed
            then
              unifToExpr (fromAtomicType loc "" . TBitVector Raw $ subSize j i)
                >>= buildAndUnify loc typ . SliceTExpr tArg (i, j)
            else
              let msg = printf "In this slice expression, the lower bound %s is not always smaller than the greater bound %s." (show i) (show j)
               in return $ reportError loc msg
        else
          let msg = printf "In this slice expression, the upper bound %s is not always is range. It can be greater than the expression size: %s" (show j) (show busSize)
           in return $ reportError loc msg

typeSelectExpr :: Pos a -> TypeConstraint -> Expr -> SizeExpr -> ExprEnv (CanFail AtomExprCand)
typeSelectExpr loc typ@(var, _) arg indexSizeExpr = do
  tArg <- expectAtomicType (raw var) arg
  tArgSize <- unifToExpr . collapseA $ getFixedSize arg . getTypeCand <$> tArg
  indexSize <- unifToExpr $ checkSizeExpr indexSizeExpr -- indexSize >= 0 is implicit
  collapseA $ buildSelect <$> tArgSize <*> tArg <*> indexSize
  where
    buildSelect :: Size -> AtomExprCand -> Size -> ExprEnv (CanFail AtomExprCand)
    buildSelect busSize (_, tArg, _) i = do
      isInBound <- unifToExpr $ i `isStrictlySmaller` busSize -- i < busSize
      if isInBound
        then
          unifToExpr (fromAtomicType loc "" TBool)
            >>= buildAndUnify loc typ . SelectTExpr tArg i
        else
          let msg = printf "In this select expression, the index %s is not always is range. It can be greater than the expression size: %s" (show i) (show busSize)
           in return $ reportError loc msg

typeConvertExpr :: Pos a -> TypeConstraint -> BitVectorKind -> Expr -> ExprEnv (CanFail AtomExprCand)
typeConvertExpr loc typ@(var, _) kind arg = do
  tArg <- expectAtomicType (rawSigOrUnsig var) arg
  tArgSize <- unifToExpr . collapseA $ getFixedSize arg . getTypeCand <$> tArg
  collapseA $ buildConvert <$> tArgSize <*> tArg
  where
    buildConvert busSize (_, tArg, _) =
      unifToExpr (fromAtomicType loc "" (TBitVector kind busSize))
        >>= buildAndUnify loc typ . ConvertTExpr tArg
