module Typing.Ast (module Typing.Ast, module Commons.AstTypes, VarIdent, NodeIdent) where

import Commons.AstTypes
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Typing.Ids (NodeIdent (), VarIdent ())

-- | Atomic type (for instance of a variable)
data AtomicTType
  = -- | The Boolean Type
    TBool
  | -- | The BitVector Type
    TBitVector BitVectorKind BVSize
  deriving (Show, Eq, Ord)

-- | Type of an Expression
data TType
  = -- | An Atomic Type (Not a Tuple)
    TAtom AtomicTType
  | -- | A Composed Type, containing multiple types
    TTuple (BiList TType)
  deriving (Show, Eq, Ord)

-- | A Typed Expression in a Lustre Program
type TExpr typ = Localized (TExprDesc typ, typ)

-- | The Description of what is an Expression
data TExprDesc typ
  = -- | A Constant Expression: @10@ or @false@
    ConstantTExpr Constant
  | -- | A Variable Expression
    VarTExpr (Localized VarIdent)
  | -- | A Unary Expression: @not e@ or @-e@
    UnOpTExpr UnOp (TExpr typ)
  | -- | A Binary Expression: @a + b@, @a <> b@, @a and b@, ...
    BinOpTExpr BinOp (TExpr typ) (TExpr typ)
  | -- | Call to another Node
    AppTExpr (Localized NodeIdent) [TExpr typ]
  | -- | A Tuple of Expression: @(a, b, c)@
    TupleTExpr (BiList (TExpr typ))
  | -- | Conditional Expression: @if c then a else b@
    IfTExpr {ifCond :: TExpr typ, ifTrue :: TExpr typ, ifFalse :: TExpr typ}
  | -- | Initialized Delay Expression: @0 fby e@
    FbyTExpr {fbyInit :: TExpr typ, fbyNext :: TExpr typ}
  deriving (Show)

exprType :: TExpr typ -> typ
exprType = snd . unwrap

-- | Left hand-side of a Lustre Equation
data TPattern
  = -- | An Atomic Pattern, define a single variable
    TPatAtomic (Localized VarIdent)
  | -- | A Composed Pattern, define multiples variables
    TPatTuple (BiList TPattern)
  deriving (Show)

-- | A Lustre Equation, variables of a Pattern are defined with a Lustre Expression
type TEquation typ = (TPattern, TExpr typ)

-- | A Typed Lustre Node
data TNode = TNode NodeContext [TEquation TType]
  deriving (Show)

data NodeContext = NodeContext
  { -- | List of the Input Variables of a Node
    tnodeInput :: [Localized VarIdent],
    -- | List of the Output Variables of a Node
    tnodeOutput :: NonEmpty (Localized VarIdent),
    -- | List of the Local Variables of a Node
    tnodeLocal :: [Localized VarIdent],
    -- | Mapping from the Node's variables to their Types
    tnodeVarTypes :: Map VarIdent AtomicTType
  }
  deriving (Show)

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

-- | A Typed Lustre Program
data TAst = TAst
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    tastNodeDecl :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    tastNodes :: [(NodeIdent, Localized TNode)]
  }
  deriving (Show)
