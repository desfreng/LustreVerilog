{-# LANGUAGE InstanceSigs #-}

module Commons.BiList (BiList (..), zipWith, zipWithM, unzip) where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Foldable (Foldable (toList))
import Data.Foldable1
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
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

instance Foldable1 BiList where
  foldMap1 :: (Semigroup m) => (a -> m) -> BiList a -> m
  foldMap1 f (BiList x y l) = foldl go (f x <> f y) l
    where
      go acc elm = acc <> f elm

  toNonEmpty :: BiList a -> NonEmpty a
  toNonEmpty (BiList x y l) = x :| y : l

instance Traversable BiList where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> BiList a -> f (BiList b)
  traverse f (BiList x y l) = BiList <$> f x <*> f y <*> traverse f l

unzip :: BiList (a, b) -> (BiList a, BiList b)
unzip (BiList (x1, x2) (y1, y2) l) =
  let (l1, l2) = List.unzip l
   in (BiList x1 y1 l1, BiList x2 y2 l2)

listZipWithM :: (Applicative f) => (a -> b -> f c) -> [a] -> [b] -> f (Maybe [c])
listZipWithM _ [] [] = pure $ Just []
listZipWithM f (x : l) (x' : l') = fmap <$> ((:) <$> f x x') <*> listZipWithM f l l'
listZipWithM _ [] (_ : _) = pure Nothing
listZipWithM _ (_ : _) [] = pure Nothing

zipWith :: (a -> b -> c) -> BiList a -> BiList b -> Maybe (BiList c)
zipWith f b1 b2 = runIdentity $ zipWithM f' b1 b2
  where
    {-# INLINEABLE f' #-}
    f' x y = pure $ f x y

zipWithM :: (Applicative f) => (a -> b -> f c) -> BiList a -> BiList b -> f (Maybe (BiList c))
zipWithM f (BiList x1 y1 l1) (BiList x2 y2 l2) =
  fmap <$> (BiList <$> f x1 x2 <*> f y1 y2) <*> listZipWithM f l1 l2
