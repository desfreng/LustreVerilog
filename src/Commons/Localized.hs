{-# LANGUAGE InstanceSigs #-}

module Commons.Localized where

data Localized a = L Int a Int

instance (Show a) => Show (Localized a) where
  show :: (Show a) => Localized a -> String
  show (L _ v _) = show v

instance (Eq a) => Eq (Localized a) where
  (==) :: (Eq a) => Localized a -> Localized a -> Bool
  (L _ a _) == (L _ b _) = a == b

instance (Ord a) => Ord (Localized a) where
  compare :: (Ord a) => Localized a -> Localized a -> Ordering
  compare (L _ a _) (L _ b _) = compare a b

instance Functor Localized where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> Localized a -> Localized b
  fmap f (L beg x end) = L beg (f x) end

instance Foldable Localized where
  {-# INLINEABLE foldMap #-}
  foldMap :: (Monoid m) => (a -> m) -> Localized a -> m
  foldMap f (L _ x _) = f x

  {-# INLINEABLE foldr #-}
  foldr :: (a -> b -> b) -> b -> Localized a -> b
  foldr f acc (L _ x _) = f x acc

instance Traversable Localized where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> Localized a -> f (Localized b)
  traverse f (L beg x end) = enclose <$> f x
    where
      enclose y = L beg y end

{-# INLINEABLE unwrap #-}
unwrap :: Localized a -> a
unwrap (L _ x _) = x
