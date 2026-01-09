{-# LANGUAGE InstanceSigs #-}

module Commons.Ast where

import Commons.Ids (NodeIdent, SizeIdent, VarIdent)
import Commons.Size (SimpleSize, Size, constantSize, sumSize)
import Commons.Types (AtomicTType)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.RatioInt (RatioInt)

data Constant = BoolConst Bool | IntegerConst Integer
  deriving (Show, Eq)

data UnOp = UnNot | UnNeg
  deriving (Eq, Ord)

instance Show UnOp where
  show :: UnOp -> String
  show UnNeg = "neg"
  show UnNot = "not"

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
  deriving (Eq, Ord)

instance Show BinOp where
  show :: BinOp -> String
  show BinEq = "eq"
  show BinNeq = "neq"
  show BinLt = "lt"
  show BinLe = "le"
  show BinGt = "gt"
  show BinGe = "ge"
  show BinAdd = "add"
  show BinSub = "sub"
  show BinAnd = "and"
  show BinOr = "or"

data SizeConstraint a
  = EqConstr a a
  | GeqConstr a a
  | LeqConstr a a
  | GtConstr a a
  | LtConstr a a

instance (Show a) => Show (SizeConstraint a) where
  show :: (Show a) => SizeConstraint a -> String
  show (EqConstr lhs rhs) = show lhs <> " == " <> show rhs
  show (GeqConstr lhs rhs) = show lhs <> " >= " <> show rhs
  show (LeqConstr lhs rhs) = show lhs <> " <= " <> show rhs
  show (LtConstr lhs rhs) = show lhs <> " < " <> show rhs
  show (GtConstr lhs rhs) = show lhs <> " > " <> show rhs

instance Functor SizeConstraint where
  fmap :: (a -> b) -> SizeConstraint a -> SizeConstraint b
  fmap f (EqConstr lhs rhs) = EqConstr (f lhs) (f rhs)
  fmap f (GeqConstr lhs rhs) = GeqConstr (f lhs) (f rhs)
  fmap f (LeqConstr lhs rhs) = LeqConstr (f lhs) (f rhs)
  fmap f (LtConstr lhs rhs) = LtConstr (f lhs) (f rhs)
  fmap f (GtConstr lhs rhs) = GtConstr (f lhs) (f rhs)

ltToLeq :: Size -> Size -> SizeConstraint Size
ltToLeq lhs = LeqConstr (sumSize (constantSize 1) lhs)

gtToGeq :: Size -> Size -> SizeConstraint Size
gtToGeq lhs rhs = GeqConstr lhs (sumSize (constantSize 1) rhs)

-- | The Type Signature of a Lustre Node
data NodeSignature = NodeSignature
  { -- | Arity of the Node
    nodeArity :: Int,
    -- | List of the type of the Input Variables of a Node
    inputTypes :: [(VarIdent, AtomicTType)],
    -- | Output Type of a Node
    outputTypes :: NonEmpty (VarIdent, AtomicTType),
    -- | List of the Size Variables of a Node with their minimum value
    sizeVars :: [(SizeIdent, RatioInt)],
    -- | List of the Size Constraints of a Node
    sizeConstraints :: [SizeConstraint Size]
  }
  deriving (Show)

data NodeBody var eq
  = NodeBody
  { -- | Mapping from the Node's new locals variables to their types
    bodyLocal :: Map var AtomicTType,
    -- | Equations of ty
    bodyEqs :: NonEmpty eq
  }
  deriving (Show)

data Bound a = In a | Ex a
  deriving (Show, Eq)

getBound :: Bound a -> a
getBound (In x) = x
getBound (Ex x) = x

instance Functor Bound where
  fmap :: (a -> b) -> Bound a -> Bound b
  fmap f (In x) = In $ f x
  fmap f (Ex x) = Ex $ f x

instance Foldable Bound where
  foldMap :: (Monoid m) => (a -> m) -> Bound a -> m
  foldMap f (In x) = f x
  foldMap f (Ex x) = f x

instance Traversable Bound where
  traverse :: (Applicative f) => (a -> f b) -> Bound a -> f (Bound b)
  traverse f (In x) = In <$> f x
  traverse f (Ex x) = Ex <$> f x

data Interval
  = MinoredBy (Bound RatioInt)
  | MajoredBy (Bound RatioInt)
  | Between (Bound RatioInt) (Bound RatioInt)
  deriving (Show)

data Body b
  = SimpleBody b
  | ComposedBody
      { criterion :: SimpleSize,
        branches :: [(Interval, b)]
      }
  deriving (Show)

instance Functor Body where
  fmap :: (a -> b) -> Body a -> Body b
  fmap f (SimpleBody x) = SimpleBody $ f x
  fmap f (ComposedBody c bList) = ComposedBody c $ second f <$> bList

instance Foldable Body where
  foldMap :: (Monoid m) => (a -> m) -> Body a -> m
  foldMap f (SimpleBody x) = f x
  foldMap f (ComposedBody _ bList) = foldMap (f . snd) bList

instance Traversable Body where
  traverse :: (Applicative f) => (a -> f b) -> Body a -> f (Body b)
  traverse f (SimpleBody x) = SimpleBody <$> f x
  traverse f (ComposedBody c bList) = ComposedBody c <$> traverse (sequenceA . second f) bList

-- | A Lustre Node
data Node b = Node NodeSignature (Body b)
  deriving (Show)

-- | A Generic Lustre Ast
data Ast node = Ast
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    nodeSigs :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    nodes :: [(NodeIdent, node)]
  }
  deriving (Show)
