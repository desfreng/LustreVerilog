{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Typing.NodeVarEnv
  ( -- NodeVarEnv
    NodeEnvironment (),
    NodeVarEnv (),
    addInputVariable,
    addOutputVariable,
    addLocalVariable,
    runNodeVarEnv,
  )
where

import Commons.Ast
import Commons.Ids
import Commons.Position
import Commons.Types
import Commons.TypingError
import Control.Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Typing.Environments

data VarKind = Input | Output | Local

data NodeVarState = NodeVarState {varList :: CanFail [(VarKind, VarIdent)], varMap :: VarMapping}

newtype NodeVarEnv a = NodeVarEnv (State NodeVarState a)

instance Functor NodeVarEnv where
  fmap :: (a -> b) -> NodeVarEnv a -> NodeVarEnv b
  fmap f (NodeVarEnv m) = NodeVarEnv $ f <$> m

instance Applicative NodeVarEnv where
  pure :: a -> NodeVarEnv a
  pure = NodeVarEnv . pure

  (<*>) :: NodeVarEnv (a -> b) -> NodeVarEnv a -> NodeVarEnv b
  (<*>) (NodeVarEnv f) (NodeVarEnv arg) = NodeVarEnv $ f <*> arg

instance Monad NodeVarEnv where
  (>>=) :: NodeVarEnv a -> (a -> NodeVarEnv b) -> NodeVarEnv b
  (>>=) (NodeVarEnv m) f = NodeVarEnv $ m >>= unwrapM . f
    where
      unwrapM (NodeVarEnv x) = x

addVariable :: VarKind -> Pos Ident -> CanFail AtomicTType -> NodeVarEnv ()
addVariable k vName typ = NodeVarEnv $ modify addVariable'
  where
    varId = VarIdent vName

    addVariable' s = case Map.lookup varId (varMap s) of
      -- Don't forget to carry the potential error coming from typ !
      Just _ ->
        s
          { varList =
              (<>) <$> varList s <*> addError typ vName ("Variable " <> show vName <> " is already declared.")
          }
      Nothing ->
        s
          { varList = (<>) <$> varList s <*> pure [(k, varId)],
            varMap = Map.insert varId typ (varMap s)
          }

addInputVariable :: Pos Ident -> CanFail AtomicTType -> NodeVarEnv ()
addInputVariable = addVariable Input

addOutputVariable :: Pos Ident -> CanFail AtomicTType -> NodeVarEnv ()
addOutputVariable = addVariable Output

addLocalVariable :: Pos Ident -> CanFail AtomicTType -> NodeVarEnv ()
addLocalVariable = addVariable Local

runNodeVarEnv :: NodeVarEnv () -> NodeEnvironment VarIdent
runNodeVarEnv (NodeVarEnv m) =
  let s = execState m NodeVarState {varList = pure [], varMap = Map.empty}
      vL = filterList <$> varList s
      vM = sequenceA (varMap s)
   in NodeEnvironment (buildCtx <$> vL <*> vM) (varMap s)
  where
    filterList l =
      let (inL, outL, locL) = foldl f (mempty, mempty, mempty) l
       in (inL, NonEmpty.fromList outL, locL)

    buildCtx (inL, outL, locL) = NodeContext inL outL locL

    f (inL, outL, locL) (Input, v) = (inL <> [v], outL, locL)
    f (inL, outL, locL) (Output, v) = (inL, outL <> [v], locL)
    f (inL, outL, locL) (Local, v) = (inL, outL, Set.insert v locL)
