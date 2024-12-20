{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing.Ast where

import Commons.Ast
import Commons.BiList
import Commons.Localized
import Commons.Tree
import Data.List.NonEmpty

data LustreType
  = BoolType
  | BitVectorType BitVectorKind BVSize
  deriving (Show, Eq, Ord)

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

type Pattern = Tree (Localized Ident)

data Equation = Equation Pattern Expr
  deriving (Show, Eq)

data IdentDecl = IdentDecl (Localized Ident) (Localized LustreType)
  deriving (Show, Eq)

data Node = Node
  { nodeName :: Localized Ident,
    nodeInputs :: [IdentDecl],
    nodeOutputs :: NonEmpty IdentDecl,
    nodeLocals :: [IdentDecl],
    nodeEqs :: [Equation]
  }
  deriving (Show, Eq)

newtype Ast = Ast [Node]
  deriving (Show, Eq)
