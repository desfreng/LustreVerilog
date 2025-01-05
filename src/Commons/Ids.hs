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

-- | Unique Identifier of a Node in a Lustre Program
newtype NodeIdent = NodeIdent (Pos Ident)
  deriving (Eq, Ord)

instance Show NodeIdent where
  show :: NodeIdent -> String
  show (NodeIdent nId) = show nId

-- | Identifier of a variable in a Node/Function
newtype VarIdent = VarIdent (Pos Ident)
  deriving (Eq, Ord)

instance Show VarIdent where
  show :: VarIdent -> String
  show (VarIdent vId) = show vId

-- | Unique Identifier of the occurrence of a variable
data VarId
  = -- | Refers to the variable in argument
    FromIdent VarIdent
  | -- | Refers to a variable that have been added during the normalisation of an expression
    VarFbyDefinition VarIdent Int
  | -- | Refers to a variable that have been added during the normalisation of an if-expression
    VarIfCondition VarIdent Int
  | -- | Refers to a variable that have been added during the normalisation of an node call
    OutputCallVar NodeIdent Int
  deriving (Eq, Ord)

instance Show VarId where
  show :: VarId -> String
  show (FromIdent vId) = show vId
  show (VarFbyDefinition vName vId) = show vName <> "_fby_" <> show vId
  show (VarIfCondition vName vId) = show vName <> "_cond_" <> show vId
  show (OutputCallVar nId vId) = "out_" <> show nId <> "_" <> show vId
