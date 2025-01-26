{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Typing.Ast where

import Commons.Ast (Ast, BinOp, Constant, Node, UnOp)
import Commons.Ids (NodeIdent, VarId)
import Commons.Types (AtomicTType)
import Data.List.NonEmpty (NonEmpty)

-- | A Typed Expression in a Lustre Program
data TExpr atyp
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

data TEquation atyp
  = -- | The simplest equation possible: @x = e@
    SimpleTEq VarId (TExpr atyp)
  | -- | A delayed expression: @x = init -> next@
    FbyTEq VarId (TExpr atyp) (TExpr atyp)
  | -- | A call to another node : @(x, ..., z) = f(a, ..., b)@
    CallTEq (NonEmpty VarId) NodeIdent [VarId]
  deriving (Show)

type TNodeEq = TEquation AtomicTType

type TNode = Node VarId TNodeEq

type TAst = Ast TNode

exprType :: TExpr atyp -> atyp
exprType (ConstantTExpr _ t) = t
exprType (VarTExpr _ t) = t
exprType (UnOpTExpr _ _ t) = t
exprType (BinOpTExpr _ _ _ t) = t
exprType (IfTExpr _ _ _ t) = t

instance Foldable TExpr where
  foldMap :: (Monoid m) => (a -> m) -> TExpr a -> m
  foldMap f = f . exprType

instance Functor TExpr where
  fmap :: (a -> b) -> TExpr a -> TExpr b
  fmap f (ConstantTExpr cst typ) = ConstantTExpr cst $ f typ
  fmap f (VarTExpr v typ) = VarTExpr v $ f typ
  fmap f (UnOpTExpr op arg typ) = UnOpTExpr op (fmap f arg) $ f typ
  fmap f (BinOpTExpr op lhs rhs typ) = BinOpTExpr op (fmap f lhs) (fmap f rhs) $ f typ
  fmap f (IfTExpr {ifCond, ifTrue, ifFalse, ifTyp}) = IfTExpr ifCond (fmap f ifTrue) (fmap f ifFalse) $ f ifTyp

instance Traversable TExpr where
  traverse :: (Applicative f) => (a -> f b) -> TExpr a -> f (TExpr b)
  traverse f (ConstantTExpr cst typ) = ConstantTExpr cst <$> f typ
  traverse f (VarTExpr v typ) = VarTExpr v <$> f typ
  traverse f (UnOpTExpr op arg typ) = UnOpTExpr op <$> traverse f arg <*> f typ
  traverse f (BinOpTExpr op lhs rhs typ) = BinOpTExpr op <$> traverse f lhs <*> traverse f rhs <*> f typ
  traverse f (IfTExpr {ifCond, ifTrue, ifFalse, ifTyp}) =
    IfTExpr ifCond <$> traverse f ifTrue <*> traverse f ifFalse <*> f ifTyp

instance Foldable TEquation where
  foldMap :: (Monoid m) => (a -> m) -> TEquation a -> m
  foldMap f (SimpleTEq _ e) = foldMap f e
  foldMap f (FbyTEq _ initE nextE) = foldMap f initE <> foldMap f nextE
  foldMap _ (CallTEq _ _ _) = mempty

instance Functor TEquation where
  fmap :: (a -> b) -> TEquation a -> TEquation b
  fmap f (SimpleTEq v e) = SimpleTEq v $ fmap f e
  fmap f (FbyTEq v initE nextE) = FbyTEq v (fmap f initE) (fmap f nextE)
  fmap _ (CallTEq vars node e) = CallTEq vars node e

instance Traversable TEquation where
  traverse :: (Applicative f) => (a -> f b) -> TEquation a -> f (TEquation b)
  traverse f (SimpleTEq v e) = SimpleTEq v <$> traverse f e
  traverse f (FbyTEq v initE nextE) = FbyTEq v <$> traverse f initE <*> traverse f nextE
  traverse _ (CallTEq vars node e) = pure $ CallTEq vars node e
