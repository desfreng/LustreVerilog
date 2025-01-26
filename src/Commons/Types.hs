{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Commons.Types where

import Commons.Ids (TypeIdent)
import Prettyprinter (Doc, Pretty (pretty))

newtype BVSize = BVSize Int
  deriving (Show, Eq, Ord)

instance Pretty BVSize where
  pretty :: BVSize -> Doc ann
  pretty (BVSize i) = pretty i

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

typeSize :: AtomicTType -> BVSize
typeSize TBool = BVSize 1
typeSize (TBitVector _ s) = s
typeSize (TCustom _ _) = error "Not Implemented"
