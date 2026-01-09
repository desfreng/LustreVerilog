{-# LANGUAGE NamedFieldPuns #-}

module Solver.Gauss
  ( System,
    Solution (..),
    buildSystem,
    solve,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.RatioInt (RatioInt)
import qualified Data.Set as S
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import Solver.Matrix (Matrix, getCol, getElem, getRow, mapRow, mapRowWith, ncols, permuteRow, shape)
import Solver.VectorSpace (VectorSpace (..))

data Solution a
  = UniqueSolution {varMapping :: IntMap a} -- Mapping from Column variable to their solution
  | InfiniteSolutions {freeVars :: NonEmpty Int} -- Columns of the free vars
  | CheckThat {shoudBeZero :: [(Int, a)], varMapping :: IntMap a} -- Residual of one inconsistent equation
  deriving (Show)

data System a = S {m :: Matrix RatioInt, rhs :: Vector (Int, a)}

buildSystem :: Matrix RatioInt -> Vector a -> System a
buildSystem m rhs = S {m, rhs = V.indexed rhs}

swapRows :: (Int, Int) -> System a -> System a
swapRows (i, j) S {m, rhs} =
  if i == j
    then S {m, rhs}
    else S {m = permuteRow i j m, rhs = V.modify (\mL -> MV.swap mL i j) rhs}

mapSysRow :: (Int -> (Vector RatioInt, a) -> (Vector RatioInt, a)) -> System a -> System a
mapSysRow f S {m, rhs} = uncurry S $ mapRowWith go (m, rhs)
  where
    go i (r, (prov, rVal)) = let (newR, newRVal) = f i (r, rVal) in (newR, (prov, newRVal))

findPivot :: Int -> Int -> System a -> Maybe Int
findPivot startRow col S {m} =
  let colVal = getCol col m
      candidates = [i | i <- [startRow .. V.length colVal - 1], colVal V.! i /= 0]
   in case candidates of
        [] -> Nothing
        (x : _) -> Just x

gaussianElimination :: (VectorSpace a) => System a -> System a
gaussianElimination sys = rrefRecursive 0 0 sys
  where
    (nR, nC) = shape (m sys)

    rrefRecursive :: (VectorSpace a) => Int -> Int -> System a -> System a
    rrefRecursive r c system
      | r >= nR || c >= nC = system
      | otherwise =
          case findPivot r c system of
            Nothing -> rrefRecursive r (c + 1) system
            Just pivotIdx ->
              let sysSwapped = swapRows (r, pivotIdx) system
                  pivotVal = getElem r c (m sysSwapped)
                  scaledR = getScaledRow r (1 / pivotVal) sysSwapped
                  sysReduced = mapSysRow (elim (r, c) scaledR) sysSwapped
               in rrefRecursive (r + 1) (c + 1) sysReduced

    elim :: (VectorSpace a) => (Int, Int) -> (Vector RatioInt, a) -> Int -> (Vector RatioInt, a) -> (Vector RatioInt, a)
    elim (pivRowI, pivColI) (pivRow, pivRhs) rI (r, rhs)
      | pivRowI == rI = (pivRow, pivRhs)
      | otherwise =
          let fact = -(r V.! pivColI)
              scaledPivRow = (* fact) <$> pivRow
              scaledPivRhs = vScale fact pivRhs
           in (V.zipWith (+) scaledPivRow r, vSum scaledPivRhs rhs)

    getScaledRow :: (VectorSpace a) => Int -> RatioInt -> System a -> (Vector RatioInt, a)
    getScaledRow r c S {m, rhs} = ((* c) <$> getRow r m, vScale c . snd $ rhs V.! r)

findBadRow :: (VectorSpace a) => Vector (Int, a) -> (Int, Maybe Int) -> Maybe (Int, a)
findBadRow rhsV (r, Nothing) =
  let rhs = rhsV V.! r
   in if vIsNull $ snd rhs
        then Nothing -- Row of 0
        else Just rhs
findBadRow _ (_, Just _) = Nothing

determineStatus :: (VectorSpace a) => System a -> Solution a
determineStatus S {m, rhs} =
  let nC = ncols m
      rowAnalysis = V.toList $ mapRow (\i v -> (i, V.findIndex (/= 0) v)) m
      pivotCols = S.fromList (mapMaybe snd rowAnalysis)
      freeCols = filter (\c -> not (S.member c pivotCols)) [0 .. nC - 1]
      vMap = IMap.fromList [(c, snd $ rhs V.! r) | (r, Just c) <- rowAnalysis]
   in case freeCols of
        [] -> case mapMaybe (findBadRow rhs) rowAnalysis of
          [] -> UniqueSolution vMap
          l -> CheckThat l vMap
        hd : tl -> InfiniteSolutions $ hd :| tl

solve :: (VectorSpace a) => System a -> Solution a
solve rawSystem = determineStatus (gaussianElimination rawSystem)
