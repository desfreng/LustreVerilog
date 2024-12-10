{-# LANGUAGE InstanceSigs #-}

module Commons.AstTypes where

import qualified Data.List as List
import Data.Text.Lazy (Text, unpack)

newtype BVSize = BVSize Int
  deriving (Show, Eq, Ord)

data BitVectorKind = Raw | Unsigned | Signed
  deriving (Show, Eq, Ord, Enum, Bounded)

data Constant = BoolConst Bool | IntegerConst Integer
  deriving (Show, Eq)

data UnOp = UnNot | UnNeg
  deriving (Show, Eq)

data BinOp
  = BinEq
  | BinNeq
  | BinLt
  | BinLe
  | BinGt
  | BinGe
  | BinAdd
  | BinSub
  | BinAnd
  | BinOr
  deriving (Show, Eq)

newtype Ident = Ident Text
  deriving (Eq, Ord)

instance Show Ident where
  show :: Ident -> String
  show (Ident ident) = unpack ident

data Localized a = L Int a Int
  deriving (Eq, Ord)

instance (Show a) => Show (Localized a) where
  show :: (Show a) => Localized a -> String
  show (L _ v _) = show v

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

instance Traversable BiList where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> BiList a -> f (BiList b)
  traverse f (BiList x y l) = BiList <$> f x <*> f y <*> traverse f l

unbzip :: BiList (a, b) -> (BiList a, BiList b)
unbzip (BiList (x1, x2) (y1, y2) l) =
  let (l1, l2) = List.unzip l
   in (BiList x1 y1 l1, BiList x2 y2 l2)

bzip :: BiList a -> BiList b -> BiList (a, b)
bzip (BiList x1 y1 l1) (BiList x2 y2 l2) = BiList (x1, x2) (y1, y2) $ List.zip l1 l2

biListToList :: BiList a -> [a]
biListToList (BiList x y l) = x : y : l