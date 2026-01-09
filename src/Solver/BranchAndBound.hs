{-# LANGUAGE NamedFieldPuns #-}

module Solver.BranchAndBound (solveMILP) where

import Control.Monad
import Control.Monad.State
import Data.Ord
import Data.RatioInt
import Solver.Data
import Solver.Interface
import Solver.Tableau

isInt :: RatioInt -> Bool
isInt = (== 1) . denominator

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just a : _) = Just a
firstJust (Nothing : l) = firstJust l

checkValidResult :: OptimalResult -> Maybe (VarID, RatioInt)
checkValidResult OptimalResult {variablesValues} =
  firstJust $ checkVar <$> variablesValues
  where
    checkVar :: VariableResult -> Maybe (VarID, RatioInt)
    checkVar VariableResult {resType = IntegerVar, resVal, resIndex} =
      if isInt resVal
        then Nothing
        else Just (resIndex, resVal)
    checkVar VariableResult {resType = RealVar} = Nothing

branchingProblems :: Problem -> (VarID, RatioInt) -> (Problem, Problem)
branchingProblems pb (v, r) =
  let (rLow, rHigh) = (floor r, ceiling r)
      pbLow = changeProblem pb . suchThat $ Var v <=. (rLow % 1)
      pbHigh = changeProblem pb . suchThat $ Var v >=. (rHigh % 1)
   in (pbLow, pbHigh)

branch :: Problem -> State (Maybe OptimalResult) ()
branch pb@Problem {objectiveType} =
  case solveLinearProgram pb of
    Unbounded -> pure ()
    Infeasible -> pure ()
    Optimal res ->
      case checkValidResult res of
        Nothing -> do
          s <- get
          when (res `isBetter` s) . put $ Just res
        Just bInfo ->
          let (b1, b2) = branchingProblems pb bInfo
           in branch b1 >> branch b2
  where
    isBetter :: OptimalResult -> Maybe OptimalResult -> Bool
    isBetter _ Nothing = True
    isBetter new (Just curr) =
      case (comparing optimalCost new curr, objectiveType) of
        (GT, Maximize) -> True
        (LT, Minimize) -> True
        (_, _) -> False

solveMILP :: Problem -> Result
solveMILP pb =
  case solveLinearProgram pb of
    Unbounded -> Unbounded
    Infeasible -> Infeasible
    Optimal res ->
      case checkValidResult res of
        Nothing -> Optimal res
        Just bInfo ->
          let (b1, b2) = branchingProblems pb bInfo
           in maybe Infeasible Optimal $
                execState (do branch b1 >> branch b2) Nothing
