{-# LANGUAGE InstanceSigs #-}

module Commons.Position where

data Pos a = L Int a Int

instance (Show a) => Show (Pos a) where
  show :: (Show a) => Pos a -> String
  show (L _ v _) = show v

instance (Eq a) => Eq (Pos a) where
  (==) :: (Eq a) => Pos a -> Pos a -> Bool
  (L _ a _) == (L _ b _) = a == b

instance (Ord a) => Ord (Pos a) where
  compare :: (Ord a) => Pos a -> Pos a -> Ordering
  compare (L _ a _) (L _ b _) = compare a b

instance Functor Pos where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> Pos a -> Pos b
  fmap f (L beg x end) = L beg (f x) end

instance Foldable Pos where
  {-# INLINEABLE foldMap #-}
  foldMap :: (Monoid m) => (a -> m) -> Pos a -> m
  foldMap f (L _ x _) = f x

  {-# INLINEABLE foldr #-}
  foldr :: (a -> b -> b) -> b -> Pos a -> b
  foldr f acc (L _ x _) = f x acc

instance Traversable Pos where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> Pos a -> f (Pos b)
  traverse f (L beg x end) = enclose <$> f x
    where
      enclose y = L beg y end

{-# INLINEABLE unwrap #-}
unwrap :: Pos a -> a
unwrap (L _ x _) = x
