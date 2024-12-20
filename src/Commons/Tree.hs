{-# LANGUAGE InstanceSigs #-}

module Commons.Tree where

import Commons.BiList (BiList (..))
import qualified Commons.BiList as BiList
import Prelude hiding (zipWith)

data Tree a = TreeLeaf a | TreeNode (BiList (Tree a))
  deriving (Show, Eq, Ord)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (TreeLeaf a) = TreeLeaf (f a)
  fmap f (TreeNode l) = TreeNode $ fmap (fmap f) l

instance Semigroup (Tree a) where
  (<>) :: Tree a -> Tree a -> Tree a
  a <> b = TreeNode $ BiList a b []

instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f (TreeLeaf x) = f x
  foldMap f (TreeNode l) = foldMap (foldMap f) l

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (TreeLeaf x) = TreeLeaf <$> f x
  traverse f (TreeNode l) = TreeNode <$> traverse (traverse f) l

zipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWith f (TreeLeaf a) (TreeLeaf b) = TreeLeaf $ f a b
zipWith f (TreeNode l1) (TreeNode l2) = TreeNode $ BiList.zipWith (zipWith f) l1 l2
zipWith _ _ _ = error "Not the same tree"

zipWithM :: (Applicative f) => (a -> b -> f c) -> Tree a -> Tree b -> f (Tree c)
zipWithM f (TreeLeaf a) (TreeLeaf b) = TreeLeaf <$> f a b
zipWithM f (TreeNode l1) (TreeNode l2) = TreeNode <$> BiList.zipWithM (zipWithM f) l1 l2
zipWithM _ _ _ = error "Not the same tree"
