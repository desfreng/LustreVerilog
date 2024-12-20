{-# LANGUAGE InstanceSigs #-}

module Commons.Ast where

import Commons.Localized
import Data.Text.Lazy (Text, unpack)

newtype BVSize = BVSize Int
  deriving (Show, Eq, Ord)

data BitVectorKind = Raw | Unsigned | Signed
  deriving (Show, Eq, Ord)

data Constant = BoolConst Bool | IntegerConst Integer
  deriving (Show, Eq)

data UnOp = UnNot | UnNeg
  deriving (Show, Eq)

data BinOp
  = BinEq
  | BinNeq
  | BinLt
  | BinLe
  | BinGt
  | BinGe
  | BinAdd
  | BinSub
  | BinAnd
  | BinOr
  deriving (Show, Eq)

newtype Ident = Ident Text
  deriving (Eq, Ord)

instance Show Ident where
  show :: Ident -> String
  show (Ident ident) = unpack ident

-- | Unique Identifier of custom Data Type in a Lustre Program
-- newtype TypeIdent = TypeIdent Ident
--   deriving (Show, Eq, Ord)

-- | Unique Identifier of a variable in a Node/Function
newtype VarIdent = VarIdent (Localized Ident)
  deriving (Eq, Ord)

instance Show VarIdent where
  show :: VarIdent -> String
  show (VarIdent vId) = show vId

-- | Unique Identifier of a Node in a Lustre Program
newtype NodeIdent = NodeIdent (Localized Ident)
  deriving (Eq, Ord)

-- | Unique Identifier of the occurrence of a variable
data VarId
  = -- | Refers to the variable in argument
    IsVarIdent VarIdent
  | -- | Refers to a variable that have been added during the normalisation of the variable in argument
    FromVarIdent VarIdent Int
  deriving (Show)

instance Show NodeIdent where
  show :: NodeIdent -> String
  show (NodeIdent nId) = show nId
