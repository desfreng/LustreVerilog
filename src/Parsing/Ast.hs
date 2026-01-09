module Parsing.Ast where

import Commons.Ast (BinOp, Constant, Interval, SizeConstraint, UnOp)
import Commons.BiList (BiList)
import Commons.Ids (Ident, SizeIdent)
import Commons.Position (Pos)
import Commons.Tree (Tree)
import Commons.Types (BitVectorKind)
import Data.List.NonEmpty (NonEmpty)

type SizeExpr = Pos SizeDesc

data SizeDesc
  = ConstantSize Int
  | VarSize SizeIdent
  | MulSize SizeExpr Int
  | AddSize SizeExpr SizeExpr
  | DivSize SizeExpr Int
  | SubSize SizeExpr SizeExpr
  deriving (Show)

data LustreType
  = BoolType
  | BitVectorType BitVectorKind SizeExpr
  deriving (Show)

type Expr = Pos ExprDesc

data ExprDesc
  = ConstantExpr Constant
  | IdentExpr Ident
  | UnOpExpr UnOp Expr
  | BinOpExpr BinOp Expr Expr
  | ConvertExpr BitVectorKind Expr
  | ConcatExpr Expr Expr
  | SliceExpr Expr (SizeExpr, SizeExpr)
  | SelectExpr Expr SizeExpr
  | AppExpr (Pos Ident) [Expr]
  | TupleExpr (BiList Expr)
  | IfExpr {ifCond :: Expr, ifTrue :: Expr, ifFalse :: Expr}
  | FbyExpr {fbyInit :: Expr, fbyNext :: Expr}
  deriving (Show)

type Pattern = Tree (Pos Ident)

data Equation = Equation Pattern Expr
  deriving (Show)

data IdentDecl = IdentDecl (Pos Ident) (Pos LustreType)
  deriving (Show)

data PNode = PNode
  { nodeName :: Pos Ident,
    nodeInputs :: [IdentDecl],
    nodeOutputs :: NonEmpty IdentDecl,
    nodeSize :: [Pos SizeIdent],
    nodeSizeConstraints :: [SizeConstraint SizeExpr],
    nodeBody :: PBody
  }
  deriving (Show)

data PBody
  = PSimpleBody PNodeBody
  | PComposedBody
      { whenCriterion :: Pos (Interval, SizeExpr),
        whenBody :: PNodeBody,
        otherwiseLoc :: Pos (),
        otherwiseBody :: PNodeBody
      }
  deriving (Show)

data PNodeBody = PNodeBody
  { pbodyLocals :: [IdentDecl],
    pbodyEqs :: NonEmpty Equation
  }
  deriving (Show)

newtype PAst = PAst [PNode]
  deriving (Show)
