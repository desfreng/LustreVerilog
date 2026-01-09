{-# LANGUAGE InstanceSigs #-}

module Compiling.Ast where

import Commons.Ast (BinOp, Constant, Node, NodeBody, UnOp)
import Commons.Ids (NodeIdent, VarId, varIdPrefix)
import Commons.Size (Size)
import Data.List.NonEmpty (NonEmpty)

data CConstant = CConstant Size Constant

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
  | ConcatFirst VarId Int
  | ConcatSecond VarId Int
  | SliceArg VarId Int
  | SelectArg VarId Int
  | ConvertArg VarId Int
  deriving (Eq, Ord)

type CVal = Either CConstant CVar

data CAction
  = SetValCAct CVal
  | -- | A Unary Action: @not e@ or @-e@
    UnOpCAct CUnOp CVal
  | -- | A Binary Action: @a + b@, @a <> b@, @a and b@, ...
    BinOpCAct CBinOp CVal CVal
  | -- | Conditional Action: @if c then a else b@
    IfCAct {ifCond :: VarId, ifTrue :: CVal, ifFalse :: CVal}
  | -- | Fby Action: @a fby b@
    FbyCAct {initVar :: CVal, nextVar :: CVal}
  | -- | Concat Expression: @a ++ b@
    ConcatCAct CVal CVal
  | -- | Slice Expression: @a[1:3]@
    SliceCAct CVal (Size, Size)
  | -- | Select Expression: @a[1]@
    SelectCAct CVal Size
  deriving (Show)

data CEquation
  = -- | The simplest equation possible: @x = e@
    SimpleCEq CVar CAction
  | -- | A call to another node : @(x, ..., z) = f[N, ..., M](a, ..., b)@
    CallCEq (NonEmpty CVar) NodeIdent [Size] [CVal]
  deriving (Show)

type CBody = NodeBody CVar CEquation

type CNode = Node CBody

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
  show (ConcatFirst varId cVarId) = varIdPrefix varId <> "_concat_fst_" <> show cVarId
  show (ConcatSecond varId cVarId) = varIdPrefix varId <> "_concat_snd_" <> show cVarId
  show (SliceArg varId cVarId) = varIdPrefix varId <> "_sliced_" <> show cVarId
  show (SelectArg varId cVarId) = varIdPrefix varId <> "_selected_" <> show cVarId
  show (ConvertArg varId cVarId) = varIdPrefix varId <> "_conv_" <> show cVarId

instance Show CConstant where
  show :: CConstant -> String
  show (CConstant _ cst) = show cst
