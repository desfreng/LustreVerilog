module Typing.Ast where

import Commons.Ast
import Commons.BiList
import Commons.Localized
import Commons.Tree
import Commons.Types
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)

-- | A Typed Expression in a Lustre Program
type TExpr typ atyp = Localized (TExprKind typ atyp)

-- | The Description of what is an Expression
data TExprKind typ atyp
  = -- | A Constant Expression: @10@ or @false@
    ConstantTExpr Constant atyp
  | -- | A Variable Expression
    VarTExpr VarIdent atyp
  | -- | A Unary Expression: @not e@ or @-e@
    UnOpTExpr UnOp (TExpr typ atyp) atyp
  | -- | A Binary Expression: @a + b@, @a <> b@, @a and b@, ...
    BinOpTExpr BinOp (TExpr typ atyp) (TExpr typ atyp) atyp
  | -- | Call to another Node
    AppTExpr NodeIdent [TExpr typ atyp] typ
  | -- | A Tuple of Expression: @(a, b, c)@
    TupleTExpr (BiList (TExpr typ atyp)) typ
  | -- | Conditional Expression: @if c then a else b@
    IfTExpr {ifCond :: TExpr typ atyp, ifTrue :: TExpr typ atyp, ifFalse :: TExpr typ atyp, ifTyp :: typ}
  | -- | Initialized Delay Expression: @0 fby e@
    FbyTExpr {fbyInit :: TExpr typ atyp, fbyNext :: TExpr typ atyp, fbyTyp :: typ}
  deriving (Show)

-- | Left hand-side of a Lustre Equation
type TPattern = Tree VarIdent

-- | A Lustre Equation, variables of a Pattern are defined with a Lustre Expression
type TEquation typ atyp = (TPattern, TExpr typ atyp)

type TNodeEq = TEquation TType AtomicTType

-- | A Typed Lustre Node
data TNode = TNode {tnodeCtx :: TNodeContext, tnodeEqs :: [TNodeEq]}
  deriving (Show)

data TNodeContext = TNodeContext
  { -- | List of the Input Variables of a Node
    tnodeInput :: [VarIdent],
    -- | List of the Output Variables of a Node
    tnodeOutput :: NonEmpty VarIdent,
    -- | List of the Local Variables of a Node
    tnodeLocal :: [VarIdent],
    -- | Mapping from the Node's variables to their Types
    tnodeVarTypes :: Map VarIdent AtomicTType
  }
  deriving (Show)

-- | A Typed Lustre Program
data TAst = TAst
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    tastNodeDecl :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    tastNodes :: [(NodeIdent, TNode)]
  }
  deriving (Show)
