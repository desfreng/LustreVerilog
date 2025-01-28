{-# LANGUAGE InstanceSigs #-}

module Compiling.Ast where

import Commons.Ast (BinOp, Constant, Node, UnOp)
import Commons.Ids (NodeIdent, VarId, varIdPrefix)
import Commons.Types (BVSize)
import Data.List.NonEmpty (NonEmpty)

data CConstant = CConstant BVSize Constant

data CUnOp = CUnNot | CUnNeg
  deriving (Eq, Ord)

instance Show CUnOp where
  show :: CUnOp -> String
  show CUnNeg = "neg"
  show CUnNot = "not"

data CBinOp
  = CBinEq
  | CBinSignedLt
  | CBinUnsignedLt
  | CBinAdd
  | CBinSub
  | CBinAnd
  | CBinOr
  deriving (Eq, Ord)

instance Show CBinOp where
  show :: CBinOp -> String
  show CBinAdd = "add"
  show CBinAnd = "and"
  show CBinEq = "eq"
  show CBinOr = "or"
  show CBinSignedLt = "signed_lt"
  show CBinSub = "sub"
  show CBinUnsignedLt = "unsigned_lt"

data CVar
  = FromVarId VarId
  | FbyInit VarId Int
  | FbyNext VarId Int
  | UnOpArg UnOp VarId Int
  | NotIntroduced BinOp VarId Int
  | BinOpArg BinOp VarId Int
  | IfTrueBranch VarId Int
  | IfFalseBranch VarId Int
  deriving (Eq, Ord)

data CAction
  = -- | A Constant Action: @10@ or @false@
    ConstantCAct CConstant
  | -- | A Variable Action
    VarCAct CVar
  | -- | A Unary Action: @not e@ or @-e@
    UnOpCAct CUnOp CVar
  | -- | A Binary Action: @a + b@, @a <> b@, @a and b@, ...
    BinOpCAct CBinOp CVar CVar
  | -- | Conditional Action: @if c then a else b@
    IfCAct {ifCond :: VarId, ifTrue :: CVar, ifFalse :: CVar}
  | -- | Fby Action: @a fby b@
    FbyCAct {initVar :: CVar, nextVar :: CVar}
  deriving (Show)

data CEquation
  = -- | The simplest equation possible: @x = e@
    SimpleCEq CVar CAction
  | -- | A call to another node : @(x, ..., z) = f(a, ..., b)@
    CallCEq (NonEmpty CVar) NodeIdent [CVar]
  deriving (Show)

type CNode = Node CVar CEquation

type CAst = [(NodeIdent, CNode)]

instance Show CVar where
  show :: CVar -> String
  show (FromVarId v) = show v
  show (FbyInit varId cVarId) = varIdPrefix varId <> "_init_" <> show cVarId
  show (FbyNext varId cVarId) = varIdPrefix varId <> "_next_" <> show cVarId
  show (UnOpArg op varId cVarId) = varIdPrefix varId <> "_" <> show op <> "_" <> show cVarId
  show (NotIntroduced op varId cVarId) = "not_" <> varIdPrefix varId <> "_" <> show op <> "_" <> show cVarId
  show (BinOpArg op varId cVarId) = varIdPrefix varId <> "_" <> show op <> "_" <> show cVarId
  show (IfTrueBranch varId cVarId) = varIdPrefix varId <> "_true_" <> show cVarId
  show (IfFalseBranch varId cVarId) = varIdPrefix varId <> "_false_" <> show cVarId

instance Show CConstant where
  show :: CConstant -> String
  show (CConstant _ cst) = show cst