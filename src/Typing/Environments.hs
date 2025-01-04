module Typing.Environments where

import Commons.Ast
import Commons.Ids
import Commons.Types
import Commons.TypingError
import Data.Map

data NodeEnvironment var = NodeEnvironment {nCtx :: CanFail (NodeContext var), vMapping :: VarMapping}

type VarMapping = Map VarIdent (CanFail AtomicTType)

type NodeMapping = Map NodeIdent (CanFail NodeSignature)
