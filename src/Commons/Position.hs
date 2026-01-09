{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Commons.Position
  ( Location (..),
    Pos (),
    merge,
    extendLeft,
    extendRight,
    unwrap,
    fromLoc,
    showErrorMessage,
  )
where

import Text.Printf (printf)

data Location = Location
  { line :: {-# UNPACK #-} !Int,
    col :: {-# UNPACK #-} !Int
  }

data Pos a = Pos
  { start :: {-# UNPACK #-} !Location,
    end :: {-# UNPACK #-} !Location,
    unwrap :: !a
  }

showErrorMessage :: FilePath -> Pos String -> String
showErrorMessage file Pos {start, end, unwrap} =
  let Location {line = startLine, col = startCol} = start
      Location {line = endLine, col = endCol} = end
   in if startLine == endLine
        then printf "File \"%s\", line %d, characters %d-%d: %s" file startLine startCol endCol unwrap
        else printf "File \"%s\", lines %d-%d, characters %d-%d: %s" file startLine endLine startCol endCol unwrap

merge :: (Pos a -> Pos b -> c) -> Pos a -> Pos b -> Pos c
merge f x@Pos {start} y@Pos {end} =
  Pos {start, end, unwrap = f x y}

extendLeft :: (Pos a -> b) -> Location -> Pos a -> Pos b
extendLeft f start x@Pos {end} = Pos {start, end, unwrap = f x}

extendRight :: (Pos a -> b) -> Pos a -> Location -> Pos b
extendRight f x@Pos {start} end = Pos {start, end, unwrap = f x}

{-# INLINE fromLoc #-}
fromLoc :: Location -> a -> Location -> Pos a
fromLoc start unwrap end = Pos {start, end, unwrap}

instance (Show a) => Show (Pos a) where
  show :: (Show a) => Pos a -> String
  show (Pos {unwrap}) = show unwrap

instance (Eq a) => Eq (Pos a) where
  (==) :: (Eq a) => Pos a -> Pos a -> Bool
  (Pos {unwrap = a}) == (Pos {unwrap = b}) = a == b

instance (Ord a) => Ord (Pos a) where
  compare :: (Ord a) => Pos a -> Pos a -> Ordering
  compare (Pos {unwrap = a}) (Pos {unwrap = b}) = compare a b

instance Functor Pos where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> Pos a -> Pos b
  fmap f x = x {unwrap = f $ unwrap x}

instance Foldable Pos where
  {-# INLINEABLE foldMap #-}
  foldMap :: (Monoid m) => (a -> m) -> Pos a -> m
  foldMap f x = f $ unwrap x

  {-# INLINEABLE foldr #-}
  foldr :: (a -> b -> b) -> b -> Pos a -> b
  foldr f acc x = f (unwrap x) acc

instance Traversable Pos where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> Pos a -> f (Pos b)
  traverse f x = enclose <$> f (unwrap x)
    where
      enclose y = x {unwrap = y}
