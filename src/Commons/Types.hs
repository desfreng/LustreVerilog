module Commons.Types where

import Commons.Ast (BVSize (..), BitVectorKind (..))
import Commons.Tree
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)

-- | Atomic type (for instance of a variable)
data AtomicTType
  = -- | The Boolean Type
    TBool
  | -- | The BitVector Type
    TBitVector BitVectorKind BVSize
  | -- | The BitVector Type
    TCustom () [AtomicTType]
  deriving (Eq, Ord)

instance Show AtomicTType where
  show TBool = "bool"
  show (TBitVector Raw (BVSize s)) = "r" <> show s
  show (TBitVector Unsigned (BVSize s)) = "u" <> show s
  show (TBitVector Signed (BVSize s)) = "i" <> show s
  show (TCustom x args) =
    let strArgs = (\t -> "(" <> show t <> ")") <$> args
     in "custom " <> show x <> " " <> unwords strArgs

-- | Type of an Expression
type TType = Tree AtomicTType

ppTType :: TType -> String
ppTType (TreeNode l) = "(" <> intercalate ", " (toList $ ppTType <$> l) <> ")"
ppTType (TreeLeaf atom) = show atom

ppAtomicType :: AtomicTType -> String
ppAtomicType = show

-- | The Type Signature of a Lustre Node
data NodeSignature = NodeSignature
  { -- | Arity of the Node
    nodeArity :: Int,
    -- | List of the type of the Input Variables of a Node
    inputTypes :: [AtomicTType],
    -- | Output Type of a Node
    outputType :: TType
  }
  deriving (Show)
