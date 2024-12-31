module Typing.Ast where

import Commons.Ast
import Commons.BiList
import Commons.Ids
import Commons.Position
import Commons.Tree
import Commons.Types

-- | A Typed Expression in a Lustre Program
type TExpr typ atyp = Pos (TExprKind typ atyp)

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
  | -- | Conditional Expression: @if c then a else b@
    IfTExpr (TExpr typ atyp) (TExpr typ atyp) (TExpr typ atyp) typ
  | -- | Call to another Node
    AppTExpr NodeIdent [TExpr typ atyp] typ
  | -- | A Tuple of Expression: @(a, b, c)@
    TupleTExpr (BiList (TExpr typ atyp)) typ
  | -- | Initialized Delay Expression: @0 fby e@
    FbyTExpr {fbyInit :: TExpr typ atyp, fbyNext :: TExpr typ atyp, fbyTyp :: typ}
  deriving (Show)

-- | Left hand-side of a Lustre Equation
type TPattern = Tree VarIdent

-- | A Lustre Equation, variables of a Pattern are defined with a Lustre Expression
type TEquation typ atyp = (TPattern, TExpr typ atyp)

type TNodeEq = TEquation TType AtomicTType

type TNode = Node VarIdent TNodeEq

type TAst = Ast TNode