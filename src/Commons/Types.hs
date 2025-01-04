{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Commons.Types where

import Commons.Ids (TypeIdent)

newtype BVSize = BVSize Int
  deriving (Show, Eq, Ord)

data BitVectorKind = Raw | Unsigned | Signed
  deriving (Show, Eq, Ord)

-- | Atomic type (for instance of a variable)
data AtomicTType
  = -- | The Boolean Type
    TBool
  | -- | The BitVector Type
    TBitVector BitVectorKind BVSize
  | -- | The BitVector Type
    TCustom TypeIdent [AtomicTType]
  deriving (Eq, Ord)

instance Show AtomicTType where
  show :: AtomicTType -> String
  show TBool = "bool"
  show (TBitVector Raw (BVSize s)) = "r" <> show s
  show (TBitVector Unsigned (BVSize s)) = "u" <> show s
  show (TBitVector Signed (BVSize s)) = "i" <> show s
  show (TCustom x args) =
    let strArgs = (\t -> "(" <> show t <> ")") <$> args
     in "custom " <> show x <> " " <> unwords strArgs
