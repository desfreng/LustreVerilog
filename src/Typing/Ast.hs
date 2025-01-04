module Typing.Ast where

import Commons.Ast (Ast, BinOp, Constant, Node, UnOp)
import Commons.Ids (NodeIdent, VarId)
import Commons.Position (Pos)
import Commons.Types (AtomicTType)
import Data.List.NonEmpty (NonEmpty)

-- | A Typed Expression in a Lustre Program
type TExpr atyp = Pos (TExprKind atyp)

-- | The Description of what is an Expression
data TExprKind atyp
  = -- | A Constant Expression: @10@ or @false@
    ConstantTExpr Constant atyp
  | -- | A Variable Expression
    VarTExpr VarId atyp
  | -- | A Unary Expression: @not e@ or @-e@
    UnOpTExpr UnOp (TExpr atyp) atyp
  | -- | A Binary Expression: @a + b@, @a <> b@, @a and b@, ...
    BinOpTExpr BinOp (TExpr atyp) (TExpr atyp) atyp
  | -- | Conditional Expression: @if c then a else b@
    IfTExpr {ifCond :: VarId, ifTrue :: (TExpr atyp), ifFalse :: (TExpr atyp), ifTyp :: atyp}
  deriving (Show)

exprType :: TExprKind atyp -> atyp
exprType (ConstantTExpr _ t) = t
exprType (VarTExpr _ t) = t
exprType (UnOpTExpr _ _ t) = t
exprType (BinOpTExpr _ _ _ t) = t
exprType (IfTExpr _ _ _ t) = t

data TEquation atyp
  = -- | The simplest equation possible: @x = e@
    SimpleEq VarId (TExpr atyp)
  | -- | A delayed expression: @x = init -> next@
    FbyEq VarId (TExpr atyp) (TExpr atyp)
  | -- | A call to another node : @(x, ..., z) = f(a, ..., b)@
    CallEq (NonEmpty VarId) NodeIdent [TExpr atyp]
  deriving (Show)

type TNodeEq = TEquation AtomicTType

type TNode = Node VarId TNodeEq

type TAst = Ast TNode