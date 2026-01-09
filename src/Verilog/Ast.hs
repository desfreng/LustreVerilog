{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Verilog.Ast where

import Commons.Ast (Body)
import Commons.Ids (Ident, NodeIdent, SizeIdent)
import Commons.Size (Size, constantSize, isNull, subSize)
import Compiling.Ast (CBinOp, CUnOp)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.RatioInt (RatioInt)
import Prettyprinter (Doc, Pretty (pretty), unsafeViaShow, (<+>))

data Constant = Constant
  { valueSize :: Size,
    value :: Integer
  }
  deriving (Show)

one :: Constant
one = Constant (constantSize 1) 1

zero :: Constant
zero = Constant (constantSize 1) 0

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

data ModuleInst = ModuleInst
  { name :: ModuleName,
    sizeArgs :: [Size],
    controlArgs :: Maybe (ModuleControl Ident),
    inArgs :: [Either Constant Ident],
    outArgs :: NonEmpty Ident
  }
  deriving (Show)

data Expr
  = AssignExpr Ident (Either Constant Ident)
  | InstExpr ModuleInst
  deriving (Show)

data VarDecl = WireDecl Size Ident
  deriving (Show)

data SizeDecl = SizeDecl SizeIdent RatioInt
  deriving (Show)

getSizeVarName :: SizeDecl -> SizeIdent
getSizeVarName (SizeDecl n _) = n

data ModuleHead = ModuleHead
  { moduleName :: ModuleName,
    moduleSize :: [SizeDecl],
    controlVars :: Maybe (ModuleControl VarDecl),
    inputVars :: [VarDecl],
    outputVars :: NonEmpty VarDecl
  }
  deriving (Show)

data Module = Module
  { moduleHead :: ModuleHead,
    moduleBody :: Body ModuleSection
  }
  deriving (Show)

data ModuleSection = ModuleSection
  { sectionLocal :: [VarDecl],
    sectionBody :: NonEmpty Expr
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
  pretty (WireDecl size name) = "wire" <+> prettyVarDecl size name

instance Show ModuleName where
  show :: ModuleName -> String
  show (Base m) = show m
  show (Custom node) = show node
  show MainModule = "main"

instance Pretty ModuleName where
  pretty :: ModuleName -> Doc ann
  pretty = unsafeViaShow

getVarName :: VarDecl -> Ident
getVarName (WireDecl _ n) = n

toIdent :: ModuleControl VarDecl -> ModuleControl Ident
toIdent ModuleControl {clockVar, initVar} = ModuleControl (getVarName clockVar) (getVarName initVar)

prettyVarDecl :: Size -> Ident -> Doc ann
prettyVarDecl size name =
  let upBound = subSize size $ constantSize 1
   in if isNull upBound
        then pretty name
        else "[" <> pretty upBound <> ":0]" <+> pretty name
