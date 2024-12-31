{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Commons.Tree where

import Commons.BiList (BiList (..))
import qualified Commons.BiList as BiList
import Control.Monad.Identity (Identity (runIdentity))
import Data.Foldable (toList)
import Data.List (intercalate)
import Prelude hiding (zipWith)

data Tree a = TreeLeaf a | TreeNode (BiList (Tree a))
  deriving (Eq, Ord)

buildShow :: [String] -> String
buildShow l = "(" <> intercalate ", " l <> ")"

instance (Show a) => Show (Tree a) where
  show :: (Show a) => Tree a -> String
  show (TreeLeaf a) = show a
  show (TreeNode l) = buildShow . toList $ show <$> l

showA :: (Applicative f) => (a -> f String) -> Tree a -> f String
showA f (TreeLeaf a) = f a
showA f (TreeNode l) = fmap (buildShow . toList) . sequenceA $ showA f <$> l

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

zipWith :: (a -> b -> Maybe c) -> Tree a -> Tree b -> Maybe (Tree c)
zipWith f t1 t2 = runIdentity $ zipWithM f' t1 t2
  where
    {-# INLINEABLE f' #-}
    f' a b = pure $ f a b

zipWithM :: (Applicative f) => (a -> b -> f (Maybe c)) -> Tree a -> Tree b -> f (Maybe (Tree c))
zipWithM f (TreeLeaf a) (TreeLeaf b) = fmap TreeLeaf <$> f a b
zipWithM f (TreeNode l1) (TreeNode l2) = fmap TreeNode <$> BiList.zipWithM (zipWithM f) l1 l2
zipWithM _ _ _ = pure $ Nothing
