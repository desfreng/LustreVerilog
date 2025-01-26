{-# LANGUAGE InstanceSigs #-}

module Commons.Ids where

import Commons.Position
import Data.Text.Lazy (Text, unpack)
import Prettyprinter (Doc, Pretty (..), unsafeViaShow)

newtype Ident = Ident Text
  deriving (Eq, Ord)

instance Show Ident where
  show :: Ident -> String
  show (Ident ident) = unpack ident

instance Pretty Ident where
  pretty :: Ident -> Doc ann
  pretty = unsafeViaShow

-- | Unique Identifier of custom Data Type in a Lustre Program
newtype TypeIdent = TypeIdent (Pos Ident)
  deriving (Show, Eq, Ord)

-- | Unique Identifier of a Node in a Lustre Program
newtype NodeIdent = NodeIdent Ident
  deriving (Eq, Ord)

instance Show NodeIdent where
  show :: NodeIdent -> String
  show (NodeIdent nId) = show nId

instance Pretty NodeIdent where
  pretty :: NodeIdent -> Doc ann
  pretty = unsafeViaShow

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
  | -- | Refers to a variable that have been added during the normalisation of an node call
    InputCallVar NodeIdent Int
  deriving (Eq, Ord)

varIdPrefix :: VarId -> String
varIdPrefix (FromIdent vId) = show vId
varIdPrefix (VarFbyDefinition vName _) = show vName <> "_fby"
varIdPrefix (VarIfCondition vName _) = show vName <> "_cond"
varIdPrefix (OutputCallVar nId _) = "out_" <> show nId
varIdPrefix (InputCallVar nId _) = "in_" <> show nId

instance Show VarId where
  show :: VarId -> String
  show (FromIdent vId) = show vId
  show v@(VarFbyDefinition _ vId) = varIdPrefix v <> "_" <> show vId
  show v@(VarIfCondition _ vId) = varIdPrefix v <> "_" <> show vId
  show v@(OutputCallVar _ vId) = varIdPrefix v <> "_" <> show vId
  show v@(InputCallVar _ vId) = varIdPrefix v <> "_" <> show vId
