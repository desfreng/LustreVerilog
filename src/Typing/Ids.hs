module Typing.Ids where

import Commons.AstTypes (Ident)

-- | Unique Identifier of custom Data Type in a Lustre Program
-- newtype TypeIdent = TypeIdent Ident
--   deriving (Show, Eq, Ord)

-- | Unique Identifier of a variable in a Node/Function
newtype VarIdent = VarIdent Ident
  deriving (Show, Eq, Ord)

-- | Unique Identifier of a Node in a Lustre Program
newtype NodeIdent = NodeIdent Ident
  deriving (Show, Eq, Ord)
