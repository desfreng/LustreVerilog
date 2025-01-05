{-# LANGUAGE InstanceSigs #-}

module Commons.Ast where

import Commons.Ids (NodeIdent, VarIdent)
import Commons.Types (AtomicTType)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)

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
    inputTypes :: [(VarIdent, AtomicTType)],
    -- | Output Type of a Node
    outputType :: NonEmpty AtomicTType
  }
  deriving (Show)

data NodeContext var = NodeContext
  { -- | List of the Input Variables of a Node
    nodeInput :: [VarIdent],
    -- | List of the Output Variables of a Node
    nodeOutput :: NonEmpty VarIdent,
    -- | Set of the Local Variables of a Node
    nodeLocal :: Set var,
    -- | Mapping from the Node's variables to their Types
    nodeVarTypes :: Map var AtomicTType
  }
  deriving (Show)

-- | A Lustre Node
data Node var eq = Node (NodeContext var) (NonEmpty eq)
  deriving (Show)

-- | A Typed Lustre Program
data Ast node = Ast
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    tastNodeDecl :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    tastNodes :: [(NodeIdent, node)]
  }
  deriving (Show)
