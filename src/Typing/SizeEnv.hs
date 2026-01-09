{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.SizeEnv
  ( SizeInfo (),
    buildNodeSizeEnv,
    checkSizeExpr,
    checkSizeInDecl,
    isSmaller,
    isZero,
    isStrictlySmaller,
    checkSizeConstraint,
    SystemResult,
    solveSystem,
    restrictToInterval,
    sizeInfoContraints,
    sizeInfoVariables,
  )
where

import Commons.Ast (Bound (..), Interval (..), NodeSignature, SizeConstraint (..), gtToGeq, ltToLeq, sizeVars)
import Commons.Error (CanFail, addError, collapseA, reportError)
import Commons.Ids (SizeIdent)
import Commons.Position (Pos (..))
import Commons.Size
import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader, reader)
import Data.Bifunctor (first)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import Data.List (intercalate)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid (Ap (..))
import Data.RatioInt (RatioInt, (%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Parsing.Ast (SizeDesc (..), SizeExpr)
import Solver.BranchAndBound (solveMILP)
import Solver.Data (OptimalResult (OptimalResult), Result (..), VarTag (..), VarType (..), VariableResult (..))
import Solver.Gauss (Solution (..), buildSystem, solve)
import Solver.Interface
import Solver.Matrix (fromRowList)
import Text.Printf (printf)

data SizeInfo = SizeInfo
  { sizeCtx :: SizeContext,
    varDefaultValue :: Map SizeIdent RatioInt
  }
  deriving (Show)

data SizeContext = SizeContext
  { sizeSet :: Set SizeIdent,
    sizeConstr :: [SizeConstraint Size]
  }
  deriving (Show)

sizeInfoContraints :: SizeInfo -> [SizeConstraint Size]
sizeInfoContraints SizeInfo {sizeCtx = SizeContext {sizeConstr}} = sizeConstr

sizeInfoVariables :: SizeInfo -> Map SizeIdent RatioInt
sizeInfoVariables SizeInfo {varDefaultValue} = varDefaultValue

exprToSizeEq :: Set SizeIdent -> SizeExpr -> CanFail Size
exprToSizeEq validVars loc = go $ unwrap loc
  where
    go :: SizeDesc -> CanFail Size
    go (VarSize v) =
      if Set.member v validVars
        then return $ varSize v
        else reportError loc $ printf "Undefined size variable: %s" $ show v
    go (ConstantSize i) =
      return $ constantSize $ i % 1
    go (MulSize e i) =
      scaleSize (i % 1) <$> exprToSizeEq validVars e
    go (DivSize e i) =
      let e' = exprToSizeEq validVars e
       in if i == 0
            then addError e' loc "Division by zero in size expression"
            else scaleSize (1 % i) <$> e'
    go (AddSize e1 e2) =
      sumSize <$> exprToSizeEq validVars e1 <*> exprToSizeEq validVars e2
    go (SubSize e1 e2) =
      subSize <$> exprToSizeEq validVars e1 <*> exprToSizeEq validVars e2

solvePB :: SizeContext -> ObjectiveType -> Size -> Result SizeIdent
solvePB SizeContext {sizeSet, sizeConstr} objType obj =
  let pb = buildProblem $ do
        vMap <- foldM genVar mempty sizeSet
        () <- mapM_ (suchThat . toILPConstr vMap) sizeConstr
        setObjective objType $ toILP vMap obj
   in solveMILP pb
  where
    genVar m v = do
      vId <- namedVar IntegerVar v
      return $ Map.insert v vId m

    toILP :: Map SizeIdent Var -> Size -> ILPExpr
    toILP vMap s =
      let (coefs, cst) = splitSize s
       in cst +. Map.foldMapWithKey (\v coef -> coef *. vMap Map.! v) (getCoeffs coefs)

    toILPConstr :: Map SizeIdent Var -> SizeConstraint Size -> Constraint
    toILPConstr vMap (EqConstr x y) = (toILP vMap x -. toILP vMap y) ==. 0
    toILPConstr vMap (LeqConstr x y) = (toILP vMap x -. toILP vMap y) <=. 0
    toILPConstr vMap (GeqConstr x y) = (toILP vMap x -. toILP vMap y) >=. 0
    toILPConstr vMap (LtConstr x y) = toILPConstr vMap (ltToLeq x y)
    toILPConstr vMap (GtConstr x y) = toILPConstr vMap (gtToGeq x y)

data PositivityResult = Negative | Null | Positive

isAlwaysPositive :: (MonadReader SizeInfo m) => Size -> m PositivityResult
isAlwaysPositive obj = reader $ \SizeInfo {sizeCtx} ->
  case solvePB sizeCtx Minimize obj of
    Infeasible -> error "This problem should be feasible."
    Unbounded -> Negative
    Optimal (OptimalResult _ res) ->
      case compare res 0 of
        EQ -> Null
        GT -> Positive
        LT -> Negative

buildNodeSizeEnv :: Pos a -> [Pos SizeIdent] -> [SizeConstraint SizeExpr] -> CanFail SizeInfo
buildNodeSizeEnv nodeLoc vList vConstr = do
  validSet <- foldM checkDuplicate Set.empty vList
  validConstrs <- mapM (convertConstraint validSet) vConstr
  let sCtx = SizeContext validSet validConstrs
  let varSum = foldMap varSize validSet
  defaultSizeVal <- checkFeasible $ solvePB sCtx Minimize varSum
  return $ SizeInfo sCtx defaultSizeVal
  where
    checkDuplicate :: Set SizeIdent -> Pos SizeIdent -> CanFail (Set SizeIdent)
    checkDuplicate seen loc =
      let sName = unwrap loc
       in if sName `Set.member` seen
            then reportError loc $ printf "Duplicate size variable definition: %s" $ show sName
            else return (Set.insert sName seen)

    convertConstraint :: Set SizeIdent -> SizeConstraint SizeExpr -> CanFail (SizeConstraint Size)
    convertConstraint s constr = case constr of
      EqConstr e1 e2 -> EqConstr <$> exprToSizeEq s e1 <*> exprToSizeEq s e2
      GeqConstr e1 e2 -> GeqConstr <$> exprToSizeEq s e1 <*> exprToSizeEq s e2
      LeqConstr e1 e2 -> LeqConstr <$> exprToSizeEq s e1 <*> exprToSizeEq s e2
      LtConstr e1 e2 -> LtConstr <$> exprToSizeEq s e1 <*> exprToSizeEq s e2
      GtConstr e1 e2 -> GtConstr <$> exprToSizeEq s e1 <*> exprToSizeEq s e2

    checkFeasible :: (Ord a) => Result a -> CanFail (Map a RatioInt)
    checkFeasible Infeasible =
      reportError nodeLoc "The size constraints for this node are not feasible. They represent the empty set."
    checkFeasible (Optimal (OptimalResult vRes _)) =
      pure $ Map.fromList (mapMaybe extractSizeValue vRes)
    checkFeasible Unbounded =
      error "This problem should be Bounded."

extractSizeValue :: VariableResult a -> Maybe (a, RatioInt)
extractSizeValue VariableResult {resTag = Tagged x, resVal} = Just (x, resVal)
extractSizeValue _ = Nothing

restrictToInterval :: SizeInfo -> SimpleSize -> Interval -> Maybe SizeInfo
restrictToInterval sInfo@SizeInfo {sizeCtx} s i =
  let newCtx = sizeCtx {sizeConstr = go i ++ sizeConstr sizeCtx}
   in case solvePB newCtx Maximize $ constantSize 0 of
        Optimal _ -> Just sInfo {sizeCtx = newCtx}
        Unbounded -> error "This problem should be bounded"
        Infeasible -> Nothing
  where
    go :: Interval -> [SizeConstraint Size]
    go (MajoredBy (In x)) = [LeqConstr (toSize s) (constantSize x)]
    go (MajoredBy (Ex x)) = [LtConstr (toSize s) (constantSize x)]
    go (MinoredBy (In x)) = [LeqConstr (constantSize x) (toSize s)]
    go (MinoredBy (Ex x)) = [LtConstr (constantSize x) (toSize s)]
    go (Between lo hi) = go (MajoredBy hi) ++ go (MinoredBy lo)

checkSizeExpr :: (MonadReader SizeInfo m) => SizeExpr -> m (CanFail Size)
checkSizeExpr e = reader $ \SizeInfo {sizeCtx} -> exprToSizeEq (sizeSet sizeCtx) e

checkSizeInDecl :: (MonadReader SizeInfo m) => SizeExpr -> m (CanFail Size)
checkSizeInDecl e = do
  sEq <- checkSizeExpr e
  collapseA $ go <$> sEq
  where
    go :: (MonadReader SizeInfo m) => Size -> m (CanFail Size)
    go sEq = do
      res <- constantSize 0 `isStrictlySmaller` sEq
      return $
        if res
          then pure sEq
          else reportError e "This size expression is not always greater or equal to 1."

-- | x `isStrictlySmaller` y checks that x < y
isStrictlySmaller :: (MonadReader SizeInfo m) => Size -> Size -> m Bool
isStrictlySmaller x y = do
  res <- isAlwaysPositive (subSize y x)
  return $ case res of
    Negative -> False
    Null -> False
    Positive -> True

-- | x `isSmaller` y checks that x <= y
isSmaller :: (MonadReader SizeInfo m) => Size -> Size -> m Bool
isSmaller x y = do
  res <- isAlwaysPositive (subSize y x)
  return $ case res of
    Negative -> False
    Null -> True
    Positive -> True

isZero :: (MonadReader SizeInfo m) => Size -> m Bool
isZero x = (&&) <$> x `isSmaller` mempty <*> mempty `isSmaller` x

checkSizeConstraint :: (MonadReader SizeInfo m) => SizeConstraint Size -> m Bool
checkSizeConstraint (EqConstr lhs rhs) = (&&) <$> rhs `isSmaller` lhs <*> lhs `isSmaller` rhs
checkSizeConstraint (GeqConstr lhs rhs) = rhs `isSmaller` lhs
checkSizeConstraint (LeqConstr lhs rhs) = lhs `isSmaller` rhs
checkSizeConstraint (LtConstr lhs rhs) = checkSizeConstraint (ltToLeq lhs rhs)
checkSizeConstraint (GtConstr lhs rhs) = checkSizeConstraint (gtToGeq lhs rhs)

type SystemResult = Map SizeIdent Size

foldMapM :: (Applicative m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = getAp <$> foldMap (Ap . f)

solveSystem :: (MonadReader SizeInfo m) => Pos a -> NodeSignature -> [Pos (Size, Size)] -> m (CanFail SystemResult)
solveSystem loc nSigs constr =
  let (lhsLines, rhsLines) = List.unzip $ go . unwrap <$> constr
      lhsMat = fromRowList lhsLines
      rhsVect = V.fromList rhsLines
   in case solve $ buildSystem lhsMat rhsVect of
        UniqueSolution vMap -> return . pure $ convert vMap
        InfiniteSolutions vIndex ->
          case (varsList !!) <$> vIndex of
            v :| [] ->
              return $
                let msg = printf "Unable to find an expression for the size variable %s." (show v)
                 in reportError loc msg
            v :| l ->
              return $
                let msg = printf "Unable to find an expression for the size variables %s and %s." (intercalate "," $ show <$> l) (show v)
                 in reportError loc msg
        CheckThat res vMap -> do
          b <- foldMapM checkIsNull res
          case catMaybes b of
            [] -> return . pure $ convert vMap
            (i, x) : _ -> do
              msg <- buildError x
              return $ reportError (constr !! i) msg
  where
    varsList = fst <$> sizeVars nSigs
    nbVars = length varsList

    go :: (Size, Size) -> (Vector RatioInt, Size)
    go (s, rhs) =
      let (coefs, cst) = splitSize s
       in (map2Vector $ getCoeffs coefs, subSize rhs $ constantSize cst)

    map2Vector :: Map SizeIdent RatioInt -> Vector RatioInt
    map2Vector m = V.fromListN nbVars $ fromMaybe 0 . (`M.lookup` m) <$> varsList

    convert :: IntMap a -> Map SizeIdent a
    convert vMap = M.fromList $ first (varsList !!) <$> IMap.toList vMap

    checkIsNull :: (MonadReader SizeInfo m) => (a, Size) -> m [Maybe (a, Size)]
    checkIsNull (i, x) = do
      b <- isZero x
      return $ if b then [Nothing] else [Just (i, x)]

    buildError :: (MonadReader SizeInfo m) => Size -> m String
    buildError x = reader $ \SizeInfo {sizeCtx} ->
      let showCtx = intercalate ", " $ show <$> sizeConstr sizeCtx
       in printf "The type equation from this argument lead to %s = 0 which we cannot assert from %s." (show x) showCtx
