{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Data where

import Commons.Size (showRatio)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RatioInt (RatioInt (..))
import Data.Set (Set)
import Data.Vector.Strict (Vector)
import Solver.Matrix (Matrix)
import Text.Printf (printf)

newtype VarID = VarID Int
  deriving (Eq, Ord)

data VarType = IntegerVar | RealVar

data ObjectiveType = Maximize | Minimize

data VarTag = NoTag | SlackVar | Tagged !String

data Problem = Problem
  { nbVars :: Int,
    objectiveType :: ObjectiveType,
    varTags :: Map VarID VarTag,
    objective :: (Map VarID RatioInt, RatioInt),
    constraints :: [(Map VarID RatioInt, RatioInt)],
    intVars :: Set VarID
  }

data SimplexTableau = SimplexTableau
  { tableau :: !(Matrix RatioInt),
    basis :: !(Vector VarID)
  }

data SimplexData = SimplexData
  { artifVars :: !(Set VarID),
    origVars :: !(Vector VarID),
    origObj :: !(Vector RatioInt)
  }

data VariableResult = VariableResult
  { resIndex :: VarID,
    resTag :: VarTag,
    resVal :: RatioInt,
    resType :: VarType
  }

data OptimalResult = OptimalResult
  { variablesValues :: [VariableResult],
    optimalCost :: !RatioInt
  }

data Result
  = Infeasible
  | Unbounded
  | Optimal !OptimalResult

showTag :: VarID -> VarTag -> String
showTag (VarID idx) NoTag = "_" <> show idx
showTag (VarID idx) SlackVar = "_slack" <> show idx
showTag _ (Tagged x) = x

formatPoly :: Map VarID VarTag -> Map VarID RatioInt -> String
formatPoly vInfo l =
  let newL = map ppMult $ M.assocs l
   in case newL of
        [] -> "0"
        ((_, s) : tl) -> unwords $ s : fmap (uncurry (<>)) tl
  where
    ppMult (v, c)
      | c == 1 = ("+ ", ppVar v)
      | c == -1 = ("", "- " <> ppVar v)
      | c >= 0 = ("+ ", showRatio c <> "×" <> ppVar v)
      | otherwise = ("", "- " <> showRatio (abs c) <> "×" <> ppVar v)

    ppVar vCol = showTag vCol $ vInfo M.! vCol

formatConstraint :: Map VarID VarTag -> (Map VarID RatioInt, RatioInt) -> String
formatConstraint vInfo (coeffs, rhs) =
  let polyStr = formatPoly vInfo coeffs
   in "    " <> polyStr <> " = " <> showRatio rhs

instance Show Problem where
  show :: Problem -> String
  show Problem {objectiveType, varTags, objective, constraints, intVars} =
    let objTypeStr = case objectiveType of
          Minimize -> "min"
          Maximize -> "max"
        objStr =
          objTypeStr
            <> " "
            <> formatPoly varTags (fst objective)
            <> if snd objective /= 0
              then " + " <> showRatio (snd objective)
              else mempty
        constraintsBlock = unlines $ formatConstraint varTags <$> constraints
        intBlocks = unlines $ foldMap (\v -> ["    " <> showTag v (varTags M.! v) <> " integer"]) intVars
     in printf "%s\n  with\n%s%s\n" objStr constraintsBlock intBlocks

-- showTableau :: SimplexTableau -> String
-- showTableau SimplexTableau {tableau, basis} =
--   let fstLine = showRow 0 ""
--       body = zipWith (\v rIdx -> showRow rIdx (showVar v)) (V.toList basis) [1 .. nrows tableau - 1]
--    in "\n" <> unlines (pad varLine fstLine body)
--   where
--     nCol = ncols tableau
--     nbVar = nCol - 1
--     showVar = show
--     varLine = ("", "", showVar . VarID <$> [0 .. nbVar - 1])
--     showRow i c = (c, showRatio $ getElem i 0 tableau, [showRatio $ getElem i j tableau | j <- [1 .. nCol - 1]])
--     pad x y z =
--       let widest =
--             (1 +) . maximum $
--               maximum . fmap length . (\(a, b, c) -> a : b : c) <$> x : y : z

--           fill str = replicate (widest - length str) ' ' <> str

--           fillLine (a, b, c) =
--             fill a
--               <> " │ "
--               <> fill b
--               <> " │ "
--               <> unwords (fmap fill c)

--           sepLine =
--             replicate (widest + 1) '─'
--               <> "┼"
--               <> replicate (widest + 2) '─'
--               <> "┼"
--               <> replicate ((nCol - 1) * (widest + 1)) '─'
--        in fillLine x : sepLine : fillLine y : sepLine : fmap fillLine z

-- instance Show SimplexTableau where
--   show :: SimplexTableau -> String
--   show = showTableau

instance Show VariableResult where
  show :: VariableResult -> String
  show VariableResult {resTag, resIndex, resVal} =
    showTag resIndex resTag <> " = " <> showRatio resVal

instance Show Result where
  show :: Result -> String
  show Infeasible = "Infeasible"
  show Unbounded = "Unbounded"
  show (Optimal res) =
    let varValues = fmap (("    " <>) . show) . variablesValues $ res
     in unlines $ ("Optimal Cost: " <> showRatio (optimalCost res)) : "  with" : varValues
