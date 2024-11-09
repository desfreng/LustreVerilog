module Parsing.Ast (module Commons.AstTypes, module Parsing.Ast) where

import Commons.AstTypes
import Data.List.NonEmpty

type Expr = Localized ExprDesc

data ExprDesc
  = ConstantExpr Constant
  | IdentExpr (Localized Ident)
  | UnOpExpr UnOp Expr
  | BinOpExpr BinOp Expr Expr
  | AppExpr (Localized Ident) [Expr]
  | TupleExpr (BiList Expr)
  | IfExpr {ifCond :: Expr, ifTrue :: Expr, ifFalse :: Expr}
  | FbyExpr {fbyInit :: Expr, fbyNext :: Expr}
  deriving (Show, Eq)

data Pattern = PatIdent (Localized Ident) | PatTuple (BiList Pattern)
  deriving (Show, Eq)

data Equation = Equation Pattern Expr
  deriving (Show, Eq)

data IdentDecl = IdentDecl (Localized Ident) (Localized AtomicType)
  deriving (Show, Eq)

data Node = Node
  { nodeName :: Localized Ident,
    nodeInputs :: [IdentDecl],
    nodeOutputs :: NonEmpty IdentDecl,
    nodeLocals :: [IdentDecl],
    nodeEqs :: [Equation]
  }
  deriving (Show, Eq)

newtype Ast = Ast [Localized Node]
  deriving (Show, Eq)
