{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Verilog.Ast where

import Commons.Ids (Ident, NodeIdent)
import Commons.Types (BVSize (..))
import Compiling.Ast (CBinOp, CUnOp)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Prettyprinter

data Constant = Constant {valueSize :: BVSize, value :: Integer}
  deriving (Show)

one :: Constant
one = Constant (BVSize 1) 1

zero :: Constant
zero = Constant (BVSize 1) 0

data BaseModule
  = UnOpModule CUnOp
  | BinOpModule CBinOp
  | FbyModule
  | IfModule
  | ConcatModule
  | SliceModule
  | SelectModule
  deriving (Eq, Ord)

data ModuleName
  = Base BaseModule
  | Custom NodeIdent
  | MainModule
  deriving (Eq, Ord)

data ModuleControl a = ModuleControl {clockVar :: a, initVar :: a}
  deriving (Show)

data StaticValue = FixedValue BVSize | DeclaredValue Ident
  deriving (Show)

data ModuleInst = ModuleInst
  { name :: ModuleName,
    staticArgs :: [StaticValue],
    controlArgs :: Maybe (ModuleControl Ident),
    inArgs :: [Ident],
    outArgs :: NonEmpty Ident
  }
  deriving (Show)

data Expr
  = VarExpr Ident Ident
  | ConstExpr Ident Constant
  | InstExpr ModuleInst
  deriving (Show)

data VarType = Signed | Unsigned
  deriving (Show)

data VarSize = FixedSize BVSize | VariableSize Ident | DeltaSize Ident Ident | SumSize Ident Ident
  deriving (Show)

data VarDecl = WireDecl VarType VarSize Ident
  deriving (Show)

data StaticDecl = StaticDecl {getStaticName :: Ident, getStaticValue :: (Maybe BVSize)}
  deriving (Show)

data ModuleHead = ModuleHead
  { moduleName :: ModuleName,
    staticVars :: [StaticDecl],
    controlVars :: Maybe (ModuleControl VarDecl),
    inputVars :: [VarDecl],
    outputVars :: NonEmpty VarDecl
  }
  deriving (Show)

data Module = Module
  { moduleHead :: ModuleHead,
    moduleLocal :: [VarDecl],
    moduleBody :: (NonEmpty Expr)
  }
  deriving (Show)

type ModuleEnv = Map ModuleName ModuleHead

type ModuleImport = FilePath

data Ast = Ast ModuleEnv [Module] [ModuleImport]
  deriving (Show)

instance Semigroup Ast where
  (<>) :: Ast -> Ast -> Ast
  (Ast l1 m1 i1) <> (Ast l2 m2 i2) = Ast (l1 <> l2) (m1 <> m2) (i1 <> i2)

instance Monoid Ast where
  mempty :: Ast
  mempty = Ast mempty mempty mempty

instance Pretty Constant where
  pretty :: Constant -> Doc ann
  pretty Constant {valueSize, value} = pretty valueSize <> "'d" <> pretty value

instance Pretty VarType where
  pretty :: VarType -> Doc ann
  pretty Signed = "signed"
  pretty Unsigned = "unsigned"

instance Show BaseModule where
  show :: BaseModule -> String
  show x =
    "lustre_" <> case x of
      (UnOpModule op) -> show op
      (BinOpModule op) -> show op
      FbyModule -> "fby"
      IfModule -> "if"
      ConcatModule -> "concat"
      SliceModule -> "slice"
      SelectModule -> "select"

instance Pretty VarDecl where
  pretty :: VarDecl -> Doc ann
  pretty (WireDecl kind size name) = "wire" <+> prettyVarDecl kind size name

instance Show ModuleName where
  show :: ModuleName -> String
  show (Base m) = show m
  show (Custom node) = show node
  show MainModule = "main"

instance Pretty ModuleName where
  pretty :: ModuleName -> Doc ann
  pretty = unsafeViaShow

getVarName :: VarDecl -> Ident
getVarName (WireDecl _ _ n) = n

toIdent :: ModuleControl VarDecl -> ModuleControl Ident
toIdent ModuleControl {clockVar, initVar} = ModuleControl (getVarName clockVar) (getVarName initVar)

prettyVarSize :: VarSize -> Maybe (Doc ann)
prettyVarSize (FixedSize (BVSize 1)) = Nothing
prettyVarSize (FixedSize (BVSize size)) = Just . brackets $ unsafeViaShow (size - 1) <> ":0"
prettyVarSize (VariableSize size) = Just . brackets $ pretty size <> "-1:0"
prettyVarSize (DeltaSize fstIndex sndIndex) = Just . brackets $ "(" <> pretty sndIndex <> "-" <> pretty fstIndex <> ":0"
prettyVarSize (SumSize x y) = Just . brackets $ "(" <> pretty x <> "+" <> pretty y <> ")-1:0"

prettyVarDecl :: VarType -> VarSize -> Ident -> Doc ann
prettyVarDecl kind size name =
  case prettyVarSize size of
    Nothing -> pretty kind <+> pretty name
    Just pp -> pretty kind <+> pp <+> pretty name
