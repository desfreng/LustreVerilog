{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Matrix
  ( Matrix,
    -- Pretty printing
    prettyMatrix,
    -- Sizes and shapes
    nrows,
    ncols,
    shape,
    rowIndices,
    colIndices,
    -- read
    getElem,
    (!),
    getRow,
    getCol,
    -- write
    setElem,
    setRow,
    setCol,
    -- Row operations
    mapRow,
    mapMatRow,
    mapRowWith,
    foldRow,
    foldCol,
    foldRowM,
    foldColM,
    transpose,
    permuteRow,
    -- Modification
    filterRow,
    filterCol,
    removeRow,
    removeCol,
    -- Create
    empty,
    addRow,
    fromRowList,
  )
where

import Control.Monad ()
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV

newtype Matrix a = Matrix {mat :: Vector (Vector a)}

instance Functor Matrix where
  fmap :: (a -> b) -> Matrix a -> Matrix b
  fmap f Matrix {mat} = Matrix $ fmap (fmap f) mat

prettyMatrix :: (Show a) => Matrix a -> String
prettyMatrix m =
  concat
    [ "┌ ",
      unwords (replicate (ncols m) blank),
      " ┐\n",
      unlines
        [ "│ " ++ unwords ((\j -> fill $ strings ! (i, j)) <$> colIndices m) ++ " │"
        | i <- rowIndices m
        ],
      "└ ",
      unwords (replicate (ncols m) blank),
      " ┘"
    ]
  where
    strings@(Matrix mat) = fmap show m
    widest = V.maximum $ fmap (V.maximum . fmap length) mat
    fill str = replicate (widest - length str) ' ' ++ str
    blank = fill ""

instance (Show a) => Show (Matrix a) where
  {-# INLINE show #-}
  show :: (Show a) => Matrix a -> String
  show = prettyMatrix

-- Sizes and shapes

{-# INLINE nrows #-}
nrows :: Matrix a -> Int
nrows Matrix {mat} = V.length mat

{-# INLINE ncols #-}
ncols :: Matrix a -> Int
ncols Matrix {mat} = maybe 0 V.length $ mat V.!? 0

{-# INLINE shape #-}
shape :: Matrix a -> (Int, Int)
shape m = (nrows m, ncols m)

rowIndices :: Matrix a -> [Int]
rowIndices m = [0 .. nrows m - 1]

colIndices :: Matrix a -> [Int]
colIndices m = [0 .. ncols m - 1]

-- Read

{-# INLINE getElem #-}
getElem :: Int -> Int -> Matrix a -> a
getElem i j Matrix {mat} = (mat V.! i) V.! j

infix 9 !

{-# INLINE (!) #-}
(!) :: Matrix a -> (Int, Int) -> a
m ! (i, j) = getElem i j m

{-# INLINE getRow #-}
getRow :: Int -> Matrix a -> Vector a
getRow i Matrix {mat} = mat V.! i

{-# INLINE getCol #-}
getCol :: Int -> Matrix a -> Vector a
getCol j Matrix {mat} = (V.! j) <$> mat

-- Safe writing
setElem :: Int -> Int -> a -> Matrix a -> Matrix a
setElem i j x Matrix {mat} =
  Matrix $
    V.modify
      (\mMat -> MV.modify mMat (V.modify (\mRow -> MV.write mRow j x)) i)
      mat

setRow :: Int -> Vector a -> Matrix a -> Matrix a
setRow i row Matrix {mat} =
  Matrix $ V.modify (\mMat -> MV.write mMat i row) mat

setCol :: Int -> Vector a -> Matrix a -> Matrix a
setCol j col Matrix {mat} =
  Matrix $
    V.zipWith (\row x -> V.modify (\mRow -> MV.write mRow j x) row) mat col

-- Row operations
mapRow :: (Int -> Vector a -> b) -> Matrix a -> Vector b
mapRow f = V.imap f . mat

mapMatRow :: (Int -> Vector a -> Vector a) -> Matrix a -> Matrix a
mapMatRow f = Matrix . mapRow f

mapRowWith :: (Int -> (Vector a, b) -> (Vector a, b)) -> (Matrix a, Vector b) -> (Matrix a, Vector b)
mapRowWith f (Matrix {mat}, v) =
  let (newM, newV) = V.unzip $ V.izipWith (\i r x -> f i (r, x)) mat v
   in (Matrix newM, newV)

foldRow :: (Int -> Vector a -> b -> b) -> b -> Matrix a -> b
foldRow f acc = V.ifoldr' f acc . mat

foldCol :: (Int -> Vector a -> b -> b) -> b -> Matrix a -> b
foldCol f acc = foldRow f acc . transpose

foldRowM :: (Monad m) => (b -> Int -> Vector a -> m b) -> b -> Matrix a -> m b
foldRowM f acc = V.ifoldM' f acc . mat

foldColM :: (Monad m) => (b -> Int -> Vector a -> m b) -> b -> Matrix a -> m b
foldColM f acc = foldRowM f acc . transpose

transpose :: Matrix a -> Matrix a
transpose m = Matrix . V.fromList $ (`getCol` m) <$> [0 .. ncols m - 1]

permuteRow :: Int -> Int -> Matrix a -> Matrix a
permuteRow rowI rowJ Matrix {mat} =
  Matrix $ V.modify (\mMat -> MV.swap mMat rowI rowJ) mat

filterCol :: (Int -> Bool) -> Matrix a -> Matrix a
filterCol f Matrix {mat} =
  Matrix $ V.map (V.ifilter (\i _ -> f i)) mat

filterRow :: (Int -> Bool) -> Matrix a -> Matrix a
filterRow f Matrix {mat} =
  Matrix $ V.ifilter (\i _ -> f i) mat

removeRow :: Int -> Matrix a -> Matrix a
removeRow r = filterRow (/= r)

removeCol :: Int -> Matrix a -> Matrix a
removeCol c = filterCol (/= c)

-- Matrix construction
empty :: Matrix a
empty = Matrix V.empty

addRow :: Matrix a -> Vector a -> Matrix a
addRow Matrix {mat} row =
  case mat V.!? 0 of
    Nothing -> Matrix $ V.singleton row
    Just row0 ->
      if V.length row0 == V.length row
        then Matrix $ V.snoc mat row
        else error "Incompatible size during row append"

fromRowList :: [Vector a] -> Matrix a
fromRowList [] = empty
fromRowList l@(hd : tl) =
  if all (\row -> V.length row == rowLength) tl
    then Matrix $ V.fromList l
    else error "Incompatible size during list building"
  where
    rowLength = V.length hd

instance Semigroup (Matrix a) where
  (<>) :: Matrix a -> Matrix a -> Matrix a
  matA@(Matrix a) <> matB@(Matrix b) =
    case (a V.!? 0, b V.!? 0) of
      (Nothing, Nothing) -> empty
      (Just _, Nothing) -> matA
      (Nothing, Just _) -> matB
      (Just rowA, Just rowB) ->
        if V.length rowA == V.length rowB
          then Matrix $ a <> b
          else error "Incompatible size during concat"

instance Monoid (Matrix a) where
  mempty :: Matrix a
  mempty = empty
