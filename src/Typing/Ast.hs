{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Typing.Ast where

import Commons.Ast (Ast, BinOp, Constant, Node, NodeBody, UnOp)
import Commons.Ids (NodeIdent, VarId)
import Commons.Size (Size)
import Commons.Types (AtomicTType)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Either (lefts)
import Data.List.NonEmpty (NonEmpty)

type TConstant atyp = (Constant, atyp)

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
    IfTExpr
      { ifCond :: VarId,
        ifTrue :: TExpr atyp,
        ifFalse :: TExpr atyp,
        ifTyp :: atyp
      }
  | -- | Concat Expression: @a ++ b@
    ConcatTExpr (TExpr atyp) (TExpr atyp) atyp
  | -- | Slice Expression: @a[1:3]@
    SliceTExpr (TExpr atyp) (Size, Size) atyp
  | -- | Select Expression: @a[1]@
    SelectTExpr (TExpr atyp) Size atyp
  | -- | Convert a BitVector to another type
    ConvertTExpr (TExpr atyp) atyp
  deriving (Show)

data TEquation atyp
  = -- | The simplest equation possible: @x = e@
    SimpleTEq VarId (TExpr atyp)
  | -- | A delayed expression: @x = init -> next@
    FbyTEq VarId (TExpr atyp) (TExpr atyp)
  | -- | A call to another node : @(x, ..., z) = f[N, ..., M](a, ..., b)@
    CallTEq (NonEmpty VarId) NodeIdent [Size] [Either (TConstant atyp) VarId]
  deriving (Show)

type TNodeEq = TEquation AtomicTType

type TBody = NodeBody VarId TNodeEq

type TNode = Node TBody

type TAst = Ast TNode

exprType :: TExpr atyp -> atyp
exprType (ConstantTExpr _ t) = t
exprType (VarTExpr _ t) = t
exprType (UnOpTExpr _ _ t) = t
exprType (BinOpTExpr _ _ _ t) = t
exprType (IfTExpr _ _ _ t) = t
exprType (ConcatTExpr _ _ t) = t
exprType (SliceTExpr _ _ t) = t
exprType (SelectTExpr _ _ t) = t
exprType (ConvertTExpr _ t) = t

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
  fmap f (ConcatTExpr lhs rhs typ) = ConcatTExpr (fmap f lhs) (fmap f rhs) $ f typ
  fmap f (SliceTExpr arg i typ) = SliceTExpr (fmap f arg) i $ f typ
  fmap f (SelectTExpr arg i typ) = SelectTExpr (fmap f arg) i $ f typ
  fmap f (ConvertTExpr arg typ) = ConvertTExpr (fmap f arg) $ f typ

instance Traversable TExpr where
  traverse :: (Applicative f) => (a -> f b) -> TExpr a -> f (TExpr b)
  traverse f (ConstantTExpr cst typ) = ConstantTExpr cst <$> f typ
  traverse f (VarTExpr v typ) = VarTExpr v <$> f typ
  traverse f (UnOpTExpr op arg typ) = UnOpTExpr op <$> traverse f arg <*> f typ
  traverse f (BinOpTExpr op lhs rhs typ) = BinOpTExpr op <$> traverse f lhs <*> traverse f rhs <*> f typ
  traverse f (IfTExpr {ifCond, ifTrue, ifFalse, ifTyp}) =
    IfTExpr ifCond <$> traverse f ifTrue <*> traverse f ifFalse <*> f ifTyp
  traverse f (ConcatTExpr lhs rhs typ) = ConcatTExpr <$> traverse f lhs <*> traverse f rhs <*> f typ
  traverse f (SliceTExpr arg i typ) = SliceTExpr <$> traverse f arg <*> pure i <*> f typ
  traverse f (SelectTExpr arg i typ) = SelectTExpr <$> traverse f arg <*> pure i <*> f typ
  traverse f (ConvertTExpr arg typ) = ConvertTExpr <$> traverse f arg <*> f typ

instance Foldable TEquation where
  foldMap :: (Monoid m) => (a -> m) -> TEquation a -> m
  foldMap f (SimpleTEq _ e) = foldMap f e
  foldMap f (FbyTEq _ initE nextE) = foldMap f initE <> foldMap f nextE
  foldMap f (CallTEq _ _ _ l) = foldMap (f . snd) $ lefts l

instance Functor TEquation where
  fmap :: (a -> b) -> TEquation a -> TEquation b
  fmap f (SimpleTEq v e) = SimpleTEq v $ fmap f e
  fmap f (FbyTEq v initE nextE) = FbyTEq v (fmap f initE) (fmap f nextE)
  fmap f (CallTEq vars node sizeArgs e) = CallTEq vars node sizeArgs $ fmap (first $ second f) e

instance Traversable TEquation where
  traverse :: (Applicative f) => (a -> f b) -> TEquation a -> f (TEquation b)
  traverse f (SimpleTEq v e) = SimpleTEq v <$> traverse f e
  traverse f (FbyTEq v initE nextE) = FbyTEq v <$> traverse f initE <*> traverse f nextE
  traverse f (CallTEq vars node sizeArgs e) = CallTEq vars node sizeArgs <$> traverse (bitraverse (bitraverse pure f) pure) e
