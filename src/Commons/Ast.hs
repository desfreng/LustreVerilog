{-# LANGUAGE InstanceSigs #-}

module Commons.Ast where

import Commons.Ids
import Commons.Types
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)

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

data NodeContext typ = TNodeContext
  { -- | List of the Input Variables of a Node
    nodeInput :: [typ],
    -- | List of the Output Variables of a Node
    nodeOutput :: NonEmpty typ,
    -- | List of the Local Variables of a Node
    nodeLocal :: [typ],
    -- | Mapping from the Node's variables to their Types
    nodeVarTypes :: Map typ AtomicTType
  }
  deriving (Show)

-- | A Lustre Node
data Node typ eq = Node (NodeContext typ) (NonEmpty eq)
  deriving (Show)

-- | A Typed Lustre Program
data Ast node = Ast
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    tastNodeDecl :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    tastNodes :: [(NodeIdent, node)]
  }
  deriving (Show)
