{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parsing.Ast where

import Commons.Ast
import Commons.BiList
import Commons.Ids
import Commons.Position
import Commons.Tree
import Commons.Types
import Data.List.NonEmpty

data LustreType
  = BoolType
  | BitVectorType BitVectorKind BVSize
  deriving (Show, Eq, Ord)

type Expr = Pos ExprDesc

data ExprDesc
  = ConstantExpr Constant
  | IdentExpr (Pos Ident)
  | UnOpExpr UnOp Expr
  | BinOpExpr BinOp Expr Expr
  | ConvertExpr BitVectorKind Expr
  | ConcatExpr Expr Expr
  | SliceExpr Expr (Int, Int)
  | SelectExpr Expr Int
  | AppExpr (Pos Ident) [Expr]
  | TupleExpr (BiList Expr)
  | IfExpr {ifCond :: Expr, ifTrue :: Expr, ifFalse :: Expr}
  | FbyExpr {fbyInit :: Expr, fbyNext :: Expr}
  deriving (Show, Eq)

type Pattern = Tree (Pos Ident)

data Equation = Equation Pattern Expr
  deriving (Show, Eq)

data IdentDecl = IdentDecl (Pos Ident) (Pos LustreType)
  deriving (Show, Eq)

data PNode = PNode
  { nodeName :: Pos Ident,
    nodeInputs :: [IdentDecl],
    nodeOutputs :: NonEmpty IdentDecl,
    nodeLocals :: [IdentDecl],
    nodeEqs :: (NonEmpty Equation)
  }
  deriving (Show, Eq)

newtype PAst = PAst [PNode]
  deriving (Show, Eq)
