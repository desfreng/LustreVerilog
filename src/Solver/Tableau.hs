{-# LANGUAGE NamedFieldPuns #-}

module Solver.Tableau (solveLinearProgram) where

import Control.Monad.ST
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Ordering (..), comparing)
import Data.RatioInt (RatioInt (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import Solver.Data
import Solver.Matrix
import Prelude hiding (EQ)

newtype TableauRow = TableauRow Int
  deriving (Eq, Show)

{-# INLINE getVarValue #-}
getVarValue :: Vector a -> VarID -> a
getVarValue v (VarID i) = v V.! i

{-# INLINE tableValue #-}
tableValue :: Vector a -> VarID -> a
tableValue v (VarID i) = v V.! (i + 1)

{-# INLINE fromIndex #-}
fromIndex :: Int -> VarID
fromIndex idx
  | idx == 0 = error "Invalid Index for a Column"
  | otherwise = VarID (idx - 1)

{-# INLINE getTableCol #-}
getTableCol :: Matrix a -> VarID -> Vector a
getTableCol m (VarID x) = getCol (x + 1) m

{-# INLINE getTableRow #-}
getTableRow :: Matrix a -> TableauRow -> Vector a
getTableRow m (TableauRow x) = getRow (x + 1) m

{-# INLINE getTableElem #-}
getTableElem :: Matrix a -> TableauRow -> VarID -> a
getTableElem m (TableauRow i) (VarID j) = getElem (i + 1) (j + 1) m

{-# INLINE isRow #-}
isRow :: Int -> TableauRow -> Bool
isRow x (TableauRow y) = x == y + 1

{-# INLINE updateBasis #-}
updateBasis :: Vector VarID -> TableauRow -> VarID -> Vector VarID
updateBasis basis (TableauRow i) v = V.modify (\mBasis -> MV.write mBasis i v) basis

{-# INLINE removeTableRow #-}
removeTableRow :: TableauRow -> Matrix a -> Matrix a
removeTableRow (TableauRow i) = removeRow (i + 1)

fillObj :: ObjectiveType -> Vector RatioInt -> SimplexTableau -> SimplexTableau
fillObj t origCost s@SimplexTableau {tableau, basis} =
  s {tableau = setRow 0 prodLine tableau}
  where
    coef = case t of
      Minimize -> 1
      Maximize -> -1

    cost = (* coef) <$> origCost

    basisOrigCost = V.map (getVarValue cost) basis
    prodLine = V.create (do v <- MV.new (ncols tableau); () <- foldColM (f v) () tableau; return v)

    f v () i col =
      let colVal = V.tail col
          p = scalProd basisOrigCost colVal
          orgCost = if i == 0 then 0 else cost V.! (i - 1)
       in MV.write v i (orgCost - p)

buildState :: Problem -> (SimplexTableau, SimplexData)
buildState Problem {nbVars, objective, constraints} =
  ( fillObj Minimize basePhase1Obj $
      SimplexTableau
        { tableau = fromRowList $ nilRow : constraintsRows,
          basis = artifVars
        },
    SimplexData
      { origVars,
        artifVars = S.fromAscList . V.toList $ artifVars,
        origObj = runST (do v <- MV.replicate numOrigVars 0; () <- flattenMap 0 v (fst objective); V.freeze v)
      }
  )
  where
    numOrigVars = nbVars
    numArtifVars = length constraints

    origVars = V.generate numOrigVars VarID
    artifVars = V.generate numArtifVars $ VarID . (numOrigVars +)

    nilRow = V.replicate (1 + numOrigVars + numArtifVars) 0
    basePhase1Obj = V.replicate numOrigVars 0 <> V.replicate numArtifVars 1

    constraintsRows = zipWith buildConstraintRow [0 ..] constraints

    buildConstraintRow :: Int -> (Map VarID RatioInt, RatioInt) -> Vector RatioInt
    buildConstraintRow i (cstrCoef, cstrVal) = runST $
      do
        v <- MV.replicate (1 + numOrigVars + numArtifVars) 0
        () <- MV.write v 0 cstrVal
        () <- flattenMap 1 v cstrCoef
        () <- MV.write v (1 + numOrigVars + i) 1
        V.freeze v

    flattenMap off row coefMap =
      mapM_ (\(VarID var, val) -> MV.write row (off + var) val) $ M.toList coefMap

{-# INLINE minIndexMayBy #-}
minIndexMayBy :: (a -> Bool) -> ((Int, a) -> (Int, a) -> Ordering) -> Vector a -> Maybe Int
minIndexMayBy filterP cmpP = fmap fst . V.ifoldl' imin Nothing
  where
    imin Nothing idx elm
      | filterP elm = Just (idx, elm)
      | otherwise = Nothing
    imin acc@(Just p) idx elm
      | filterP elm =
          if cmpP (idx, elm) p == LT
            then Just (idx, elm)
            else acc
      | otherwise = acc

findPivotCol :: SimplexTableau -> Maybe VarID
findPivotCol SimplexTableau {tableau} =
  let objCoef = V.tail $ getRow 0 tableau
   in VarID <$> minIndexMayBy (< 0) (comparing fst) objCoef

findPivotRow :: SimplexTableau -> VarID -> Maybe TableauRow
findPivotRow SimplexTableau {tableau, basis} pivCol =
  let rhsVals = V.tail $ getCol 0 tableau
      pivColVals = V.tail $ getTableCol tableau pivCol

      blandFilter (_, piv) = piv > 0

      blandCmp (id1, (rhs1, piv1)) (id2, (rhs2, piv2)) =
        let r1 = rhs1 / piv1
            r2 = rhs2 / piv2
            v1 = basis V.! id1
            v2 = basis V.! id2
         in case compare r1 r2 of
              EQ -> compare v1 v2
              other -> other
   in TableauRow <$> minIndexMayBy blandFilter blandCmp (V.zip rhsVals pivColVals)

performPivot :: Matrix RatioInt -> (TableauRow, VarID) -> Matrix RatioInt
performPivot t (pivRow, pivCol) =
  mapMatRow rowOp t
  where
    pivVal = getTableElem t pivRow pivCol
    normalizedPivRow = (/ pivVal) <$> getTableRow t pivRow
    rowOp rowIdx row
      | rowIdx `isRow` pivRow = normalizedPivRow
      | otherwise =
          let factor = tableValue row pivCol
              elmOp oldRow nPivRow = oldRow - factor * nPivRow
           in V.zipWith elmOp row normalizedPivRow

pivot :: SimplexTableau -> (TableauRow, VarID) -> SimplexTableau
pivot s@SimplexTableau {tableau, basis} (pivRow, pivCol) =
  let newBasis = updateBasis basis pivRow pivCol
      newTableau = performPivot tableau (pivRow, pivCol)
   in s {tableau = newTableau, basis = newBasis}

data StepResult
  = StepOptimal !SimplexTableau
  | StepUnbounded
  | Continue !SimplexTableau

solveStep :: SimplexTableau -> StepResult
solveStep state =
  case findPivotCol state of
    Nothing -> StepOptimal state
    Just pivotCol -> do
      case findPivotRow state pivotCol of
        Nothing -> StepUnbounded
        Just pivotRow -> do
          Continue $ pivot state (pivotRow, pivotCol)

solve :: SimplexTableau -> Maybe SimplexTableau
solve state =
  case solveStep state of
    StepOptimal result -> Just result
    StepUnbounded -> Nothing
    Continue nextState -> solve nextState

preparePhase2 :: ObjectiveType -> SimplexData -> SimplexTableau -> SimplexTableau
preparePhase2 objType d@SimplexData {artifVars, origVars, origObj} s@SimplexTableau {basis = b} =
  case TableauRow <$> V.findIndex (`S.member` artifVars) b of
    Nothing ->
      let cleanState = removeColumns artifVars s
       in fillObj objType origObj cleanState
    Just rIdx -> do
      let newState = removeVarRow rIdx origVars s
       in preparePhase2 objType d newState

removeColumns :: Set VarID -> SimplexTableau -> SimplexTableau
removeColumns artifVars s@SimplexTableau {tableau, basis} =
  s {tableau = filterCol filterP tableau, basis = basis}
  where
    filterP c = (c == 0) || not (S.member (fromIndex c) artifVars)

removeVarRow :: TableauRow -> Vector VarID -> SimplexTableau -> SimplexTableau
removeVarRow vRow origVars s@SimplexTableau {tableau, basis} =
  case nonZeroCol of
    Nothing ->
      SimplexTableau
        { tableau = removeTableRow vRow tableau,
          basis = V.ifilter (\i _ -> TableauRow i /= vRow) basis
        }
    Just vCol -> pivot s (vRow, vCol)
  where
    nonZeroCol = do
      nNulIdx <- V.findIndex id $ V.map evalVar origVars
      return $ origVars V.! nNulIdx
    evalVar vCol = getTableElem tableau vRow vCol /= 0

scalProd :: (Num a) => Vector a -> Vector a -> a
scalProd x y = V.sum (V.zipWith (*) x y)

extractVarValue :: SimplexTableau -> VarID -> RatioInt
extractVarValue SimplexTableau {tableau, basis} v =
  case V.findIndex (== v) basis of
    Nothing -> 0
    Just i -> getElem (i + 1) 0 tableau

{-# INLINE extractObjective #-}
extractObjective :: ObjectiveType -> SimplexTableau -> RatioInt
extractObjective Minimize SimplexTableau {tableau} = -getElem 0 0 tableau
extractObjective Maximize SimplexTableau {tableau} = getElem 0 0 tableau

solveLinearProgram :: Problem -> Result
solveLinearProgram problem =
  let objType = objectiveType problem
      (s, d) = buildState problem
      phase1Res = fromMaybe (error "Problem should feasible") $ solve s
   in if extractObjective Minimize phase1Res > 0
        then Infeasible
        else
          let phase2State = preparePhase2 objType d phase1Res
           in case solve phase2State of
                Nothing -> Unbounded
                Just solS ->
                  let variablesValues = mapMaybe (getVarRes (intVars problem) solS) $ M.assocs (varTags problem)
                      optimalCost = extractObjective objType solS + snd (objective problem)
                   in Optimal $ OptimalResult {variablesValues, optimalCost}
  where
    getVarRes _ _ (_, SlackVar) = Nothing
    getVarRes intVars sol (varIndex, varTag) =
      Just
        VariableResult
          { resIndex = varIndex,
            resTag = varTag,
            resVal = extractVarValue sol varIndex,
            resType = if varIndex `S.member` intVars then IntegerVar else RealVar
          }
