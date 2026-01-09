{-# LANGUAGE InstanceSigs #-}

module Commons.Types where

import Commons.Size (Size, constantSize)
import Text.Printf (printf)

data BitVectorKind = Raw | Unsigned | Signed
  deriving (Eq, Ord)

instance Show BitVectorKind where
  show :: BitVectorKind -> String
  show Raw = "r"
  show Unsigned = "u"
  show Signed = "i"

-- | Atomic type (for instance of a variable)
data AtomicTType
  = -- | The Boolean Type
    TBool
  | -- | The BitVector Type
    TBitVector BitVectorKind Size

instance Show AtomicTType where
  show :: AtomicTType -> String
  show TBool = "bool"
  show (TBitVector k s) = printf "%s<%s>" (show k) (show s)

typeSize :: AtomicTType -> Size
typeSize TBool = constantSize 1
typeSize (TBitVector _ s) = s
