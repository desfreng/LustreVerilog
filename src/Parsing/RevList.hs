{-# LANGUAGE InstanceSigs #-}

module Parsing.RevList
  ( RevList,
    singleton,
    snoc,
    toList,
    toNonEmpty,
    flatten,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)

newtype RevList a = RevList (NonEmpty a)

instance (Show a) => Show (RevList a) where
  show :: (Show a) => RevList a -> String
  show l = "RevList(" <> show (toNonEmpty l) <> ")"

instance Functor RevList where
  fmap :: (a -> b) -> RevList a -> RevList b
  fmap f (RevList l) = RevList $ f <$> l

singleton :: a -> RevList a
singleton = RevList . NE.singleton

snoc :: RevList a -> a -> RevList a
snoc (RevList a) b = RevList $ NE.cons b a

toNonEmpty :: RevList a -> NonEmpty a
toNonEmpty (RevList x) = NE.reverse x

flatten :: (Semigroup a) => RevList a -> a
flatten l = sconcat $ toNonEmpty l

toList :: RevList a -> [a]
toList = NE.toList . toNonEmpty
