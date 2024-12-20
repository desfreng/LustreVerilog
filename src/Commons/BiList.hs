{-# LANGUAGE InstanceSigs #-}

module Commons.BiList (BiList (..), zipWith, zipWithM, unzip) where

import qualified Control.Monad as Monad
import Data.Foldable (Foldable (toList))
import qualified Data.List as List
import Prelude hiding (unzip, zipWith)

data BiList a = BiList a a [a]
  deriving (Eq, Ord, Show)

instance Functor BiList where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> BiList a -> BiList b
  fmap f (BiList x y l) = BiList (f x) (f y) $ f <$> l

instance Semigroup (BiList a) where
  {-# INLINEABLE (<>) #-}
  (<>) :: BiList a -> BiList a -> BiList a
  BiList x1 x2 l <> BiList y1 y2 l' = BiList x1 x2 $ l <> (y1 : y2 : l')

instance Foldable BiList where
  {-# INLINEABLE foldMap #-}
  foldMap :: (Monoid m) => (a -> m) -> BiList a -> m
  foldMap f (BiList x y l) = f x <> f y <> foldMap f l

  {-# INLINEABLE toList #-}
  toList :: BiList a -> [a]
  toList (BiList x y l) = x : y : l

instance Traversable BiList where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> BiList a -> f (BiList b)
  traverse f (BiList x y l) = BiList <$> f x <*> f y <*> traverse f l

unzip :: BiList (a, b) -> (BiList a, BiList b)
unzip (BiList (x1, x2) (y1, y2) l) =
  let (l1, l2) = List.unzip l
   in (BiList x1 y1 l1, BiList x2 y2 l2)

zipWith :: (a -> b -> c) -> BiList a -> BiList b -> BiList c
zipWith f (BiList x1 y1 l1) (BiList x2 y2 l2) = BiList (f x1 x2) (f y1 y2) (List.zipWith f l1 l2)

zipWithM :: (Applicative f) => (a -> b -> f c) -> BiList a -> BiList b -> f (BiList c)
zipWithM f (BiList x1 y1 l1) (BiList x2 y2 l2) = BiList <$> f x1 x2 <*> f y1 y2 <*> Monad.zipWithM f l1 l2
