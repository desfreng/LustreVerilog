module Normalisation.Ast where

import Commons.Ast
import Commons.Types
import Data.List.NonEmpty
import Data.Map.Strict (Map)

data NExpr
  = -- | A Constant Expression: @10@ or @false@
    ConstantNExpr AtomicTType Constant
  | -- | A Variable Expression
    VarNExpr VarId
  | -- | A Unary Expression: @not e@ or @-e@
    UnOpNExpr AtomicTType UnOp NExpr
  | -- | A Binary Expression: @a + b@, @a <> b@, @a and b@, ...
    BinOpNExpr AtomicTType BinOp NExpr NExpr
  | -- | Conditional Expression: @if c then a else b@
    IfNExpr {ifTyp :: AtomicTType, ifCond :: NExpr, ifTrue :: NExpr, ifFalse :: NExpr}
  deriving (Show)

data NEquation
  = -- | The simplest equation possible: @x = e@
    SimpleEq VarId NExpr
  | -- | A delayed expression: @x = init -> next@
    FbyEq VarId NExpr NExpr
  | -- | A call to another node : @(x, ..., z) = f(a, ..., b)@
    CallEq (NonEmpty VarId) NodeIdent [VarId]
  deriving (Show)

-- | A Normalised Lustre Node
data NNode = NNode NNodeContext (NonEmpty NEquation)
  deriving (Show)

data NNodeContext = NNodeContext
  { -- | List of the Input Variables of a Node
    tnodeInput :: [VarIdent],
    -- | List of the Output Variables of a Node
    tnodeOutput :: NonEmpty VarIdent,
    -- | List of the Local Variables of a Node
    tnodeLocal :: [VarId],
    -- | Mapping from the Node's variables to their Types
    tnodeVarTypes :: Map VarId AtomicTType
  }
  deriving (Show)

data NAst = NAst
  { -- | Mapping from nodes of the Lustre Program to their input and output Types
    nastNodeDecl :: Map NodeIdent NodeSignature,
    -- | List of the Node of the Lustre Program
    nastNodes :: [(NodeIdent, NNode)]
  }
  deriving (Show)
