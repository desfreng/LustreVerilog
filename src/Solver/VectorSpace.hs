module Solver.VectorSpace (VectorSpace (..)) where

import Data.RatioInt (RatioInt)

class VectorSpace a where
  vScale :: RatioInt -> a -> a

  vSum :: a -> a -> a

  vIsNull :: a -> Bool

  vSub :: a -> a -> a
  vSub x y = vSum x $ vScale (-1) y
