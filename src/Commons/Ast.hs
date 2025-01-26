{-# LANGUAGE InstanceSigs #-}

module Commons.Ast where

import Commons.Ids (NodeIdent, VarIdent)
import Commons.Types (AtomicTType)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

-- | A Generic Lustre Ast
data Ast node = Ast
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    nodeSigs :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    nodes :: [(NodeIdent, node)]
  }
  deriving (Show)

newContext :: (Ord b) => NodeContext a -> (a -> b) -> Set b -> Map b AtomicTType -> NodeContext b
newContext nCtx f newLocals newVarTypes =
  let oldLocals = foldMap (Set.singleton . f) (nodeLocal nCtx)
      convertOldBinding varIdent aTyp = Map.singleton (f varIdent) aTyp
      oldVarTypes = Map.foldMapWithKey convertOldBinding (nodeVarTypes nCtx)
   in nCtx {nodeLocal = oldLocals <> newLocals, nodeVarTypes = oldVarTypes <> newVarTypes}
