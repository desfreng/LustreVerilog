{-# LANGUAGE InstanceSigs #-}

module Commons.Ids where

import Commons.Position
import Data.Text.Lazy (Text, unpack)

newtype Ident = Ident Text
  deriving (Eq, Ord)

instance Show Ident where
  show :: Ident -> String
  show (Ident ident) = unpack ident

-- | Unique Identifier of custom Data Type in a Lustre Program
newtype TypeIdent = TypeIdent (Pos Ident)
  deriving (Show, Eq, Ord)

-- | Unique Identifier of a variable in a Node/Function
newtype VarIdent = VarIdent (Pos Ident)
  deriving (Eq, Ord)

instance Show VarIdent where
  show :: VarIdent -> String
  show (VarIdent vId) = show vId

-- | Unique Identifier of a Node in a Lustre Program
newtype NodeIdent = NodeIdent (Pos Ident)
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
