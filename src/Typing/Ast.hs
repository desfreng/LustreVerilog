module Typing.Ast (module Commons.AstTypes, module Typing.Ast) where

import Commons.AstTypes
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)

-- | Unique Identifier of custom Data Type in a Lustre Program
-- newtype TypeIdent = TypeIdent Ident
--   deriving (Show, Eq, Ord)

-- | Unique Identifier of a variable in a Node/Function
newtype VarIdent = VarIdent Ident
  deriving (Show, Eq, Ord)

-- | Unique Identifier of a Node in a Lustre Program
newtype NodeIdent = NodeIdent Ident
  deriving (Show, Eq, Ord)

-- | Type of a Lustre Expression
data TType
  = -- | An Atomic Type (Not a Tuple)
    Atom AtomicType
  | -- | A Composed Type, containing multiple types
    Tuple (BiList TType)
  deriving (Show, Eq, Ord)

-- | A Typed Expression in a Lustre Program
type TExpr = Localized (TExprDesc, TType)

exprType :: TExpr -> TType
exprType (L _ (_, t) _) = t

-- | The Description of what is an Expression
data TExprDesc
  = -- | A Constant Expression: @10@ or @false@
    ConstantTExpr Constant
  | -- | A Variable Expression
    VarTExpr (Localized VarIdent)
  | -- | A Unary Expression: @not e@ or @-e@
    UnOpTExpr UnOp TExpr
  | -- | A Binary Expression: @a + b@, @a <> b@, @a and b@, ...
    BinOpTExpr BinOp TExpr TExpr
  | -- | Call to another Node
    AppTExpr (Localized NodeIdent) [TExpr]
  | -- | A Tuple of Expression: @(a, b, c)@
    TupleTExpr (BiList TExpr)
  | -- | Conditional Expression: @if c then a else b@
    IfTExpr {ifCond :: TExpr, ifTrue :: TExpr, ifFalse :: TExpr}
  | -- | Initialized Delay Expression: @0 fby e@
    FbyTExpr {fbyInit :: TExpr, fbyNext :: TExpr}
  deriving (Show)

-- | Left hand-side of a Lustre Equation
data TPattern
  = -- | An Atomic Pattern, define a single variable
    TPatAtomic (Localized VarIdent)
  | -- | A Composed Pattern, define multiples variables
    TPatTuple (BiList TPattern)
  deriving (Show)

-- | A Lustre Equation, variables of a Pattern are defined with a Lustre Expression
type TEquation = (TPattern, TExpr)

-- | A Typed Lustre Node
data TNode = TNode NodeContext [TEquation]
  deriving (Show)

data NodeContext = NodeContext
  { -- | List of the Input Variables of a Node
    tnodeInput :: [Localized VarIdent],
    -- | List of the Output Variables of a Node
    tnodeOutput :: NonEmpty (Localized VarIdent),
    -- | List of the Local Variables of a Node
    tnodeLocal :: [Localized VarIdent],
    -- | Mapping from the Node's variables to their Types
    tnodeVarTypes :: Map VarIdent AtomicType
  }
  deriving (Show)

-- | The Type Signature of a Lustre Node
data NodeSignature = NodeSignature
  { -- | Arity of the Node
    nodeArity :: Int,
    -- | List of the type of the Input Variables of a Node
    inputTypes :: [AtomicType],
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