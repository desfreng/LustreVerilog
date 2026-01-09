{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Interface
  ( ToILPExpr (toILPExpr),
    Var (..),
    ILPExpr (),
    Constraint (),
    BuildProblem (),
    ObjectiveType (..),
    maximize,
    minimize,
    setObjective,
    suchThat,
    freshVar,
    namedVar,
    (+.),
    (-.),
    (*.),
    (==.),
    (>=.),
    (<=.),
    buildProblem,
    changeProblem,
  )
where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RatioInt (RatioInt (..))
import qualified Data.Set as S
import Solver.Data
import Prelude hiding (EQ)

newtype Var = Var VarID
  deriving (Eq, Ord)

data ILPExpr = ILPExpr {coeffs :: !(Map VarID RatioInt), constant :: !RatioInt}

class ToILPExpr a where
  toILPExpr :: a -> ILPExpr

instance ToILPExpr ILPExpr where
  toILPExpr :: ILPExpr -> ILPExpr
  toILPExpr = id

instance ToILPExpr Var where
  toILPExpr :: Var -> ILPExpr
  toILPExpr = varILPExpr

instance ToILPExpr RatioInt where
  toILPExpr :: RatioInt -> ILPExpr
  toILPExpr = ILPExpr mempty

instance Semigroup ILPExpr where
  (<>) :: ILPExpr -> ILPExpr -> ILPExpr
  e1 <> e2 = e1 +. e2

instance Monoid ILPExpr where
  mempty :: ILPExpr
  mempty = toILPExpr (0 :: RatioInt)

data Constraint = LE !ILPExpr | GE !ILPExpr | EQ !ILPExpr

varILPExpr :: Var -> ILPExpr
varILPExpr (Var v) = ILPExpr (M.singleton v 1) 0

addILPExpr :: ILPExpr -> ILPExpr -> ILPExpr
addILPExpr (ILPExpr a c1) (ILPExpr b c2) =
  ILPExpr (M.unionWith (+) a b) (c1 + c2)

scaleMap :: RatioInt -> Map a RatioInt -> Map a RatioInt
scaleMap 0 _ = M.empty
scaleMap k m = fmap (k *) m

scaleILPExpr :: RatioInt -> ILPExpr -> ILPExpr
scaleILPExpr k (ILPExpr m c) =
  ILPExpr (scaleMap k m) (k * c)

infixl 6 +., -.

(+.) :: (ToILPExpr a, ToILPExpr b) => a -> b -> ILPExpr
a +. b = addILPExpr (toILPExpr a) (toILPExpr b)

(-.) :: (ToILPExpr a, ToILPExpr b) => a -> b -> ILPExpr
a -. b = a +. scaleILPExpr (-1) (toILPExpr b)

infixl 7 *.

(*.) :: (ToILPExpr a) => RatioInt -> a -> ILPExpr
k *. x = scaleILPExpr k (toILPExpr x)

infix 4 <=., >=., ==.

(<=.) :: (ToILPExpr a) => a -> RatioInt -> Constraint
a <=. b = LE (a -. b)

(>=.) :: (ToILPExpr a) => a -> RatioInt -> Constraint
a >=. b = GE (a -. b)

(==.) :: (ToILPExpr a) => a -> RatioInt -> Constraint
a ==. b = EQ (a -. b)

newtype BuildProblem a = BuildProblem (State Problem a)
  deriving (Functor, Applicative, Monad)

addVar :: VarType -> VarTag -> BuildProblem Var
addVar vKind vTag = BuildProblem $ state pbAddVar
  where
    pbAddVar st@Problem {nbVars, intVars, varTags} =
      let v = VarID nbVars
          newS =
            st
              { nbVars = nbVars + 1,
                varTags = M.insert v vTag varTags,
                intVars = case vKind of
                  IntegerVar -> S.insert v intVars
                  RealVar -> intVars
              }
       in (Var v, newS)

namedVar :: VarType -> String -> BuildProblem Var
namedVar vKind = addVar vKind . Tagged

freshVar :: VarType -> BuildProblem Var
freshVar vKind = addVar vKind NoTag

normILPExpr :: Map a RatioInt -> RatioInt -> (Map a RatioInt, RatioInt)
normILPExpr c k
  | k < 0 = (scaleMap (-1) c, -k)
  | otherwise = (c, k)

toEqualZero :: Constraint -> BuildProblem (Map VarID RatioInt, RatioInt)
toEqualZero (EQ ILPExpr {coeffs, constant}) = pure $ normILPExpr coeffs (-constant)
toEqualZero (LE e) = do
  slack <- addVar RealVar SlackVar
  let ILPExpr {coeffs, constant} = e +. slack
  return $ normILPExpr coeffs (-constant)
toEqualZero (GE e) = do
  slack <- addVar RealVar SlackVar
  let ILPExpr {coeffs, constant} = e -. slack
  return $ normILPExpr coeffs (-constant)

suchThat :: Constraint -> BuildProblem ()
suchThat c = do
  cEqZ <- toEqualZero c
  BuildProblem $ modify' (\st@Problem {constraints} -> st {constraints = cEqZ : constraints})

setObjective :: ObjectiveType -> ILPExpr -> BuildProblem ()
setObjective t e =
  BuildProblem $ modify' (\st -> st {objective = (coeffs e, constant e), objectiveType = t})

maximize :: ILPExpr -> BuildProblem ()
maximize = setObjective Maximize

minimize :: ILPExpr -> BuildProblem ()
minimize = setObjective Minimize

initialProblem :: Problem
initialProblem =
  Problem
    { nbVars = 0,
      objectiveType = Minimize,
      varTags = M.empty,
      objective = (M.empty, 0),
      constraints = [],
      intVars = S.empty
    }

buildProblem :: BuildProblem () -> Problem
buildProblem (BuildProblem m) = execState m initialProblem

changeProblem :: Problem -> BuildProblem () -> Problem
changeProblem s (BuildProblem m) = execState m s
