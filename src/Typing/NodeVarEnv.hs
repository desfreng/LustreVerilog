{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Typing.NodeVarEnv
  ( -- NodeVarEnv
    VarMapping (..),
    NodeVarEnv (),
    addInputVariable,
    addOutputVariable,
    addLocalVariable,
    runNodeVarEnv,
    addLocals,
    toSizeEq,
  )
where

import Commons.Ast (NodeSignature (..))
import Commons.Error (CanFail, addError)
import Commons.Ids (Ident, VarIdent (..))
import Commons.Position (Pos)
import Commons.Size (Size)
import Commons.Types (AtomicTType)
import Control.Monad.Reader
import Control.Monad.State (State, execState, modify)
import Data.List (partition)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Parsing.Ast (SizeExpr)
import Typing.SizeEnv (SizeInfo, checkSizeInDecl, sizeInfoContraints, sizeInfoVariables)

data VarMapping
  = VarMapping
  { topLevelVars :: Map VarIdent AtomicTType,
    localVars :: Map VarIdent AtomicTType
  }

data VarKind = Input | Output | Local
  deriving (Eq)

data NodeVarState = NodeVarState
  { varList :: CanFail [(VarKind, VarIdent)],
    varMap :: Map VarIdent (CanFail AtomicTType)
  }

newtype NodeVarEnv a = NodeVarEnv (ReaderT SizeInfo (State NodeVarState) a)
  deriving (Functor, Applicative, Monad)

toSizeEq :: SizeExpr -> NodeVarEnv (CanFail Size)
toSizeEq = NodeVarEnv . checkSizeInDecl

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

runNodeVarEnv :: NodeVarEnv () -> SizeInfo -> CanFail (NodeSignature, VarMapping)
runNodeVarEnv (NodeVarEnv m) sInfo =
  let initState = NodeVarState {varList = pure [], varMap = Map.empty}
      NodeVarState {varMap, varList} = execState (runReaderT m sInfo) initState
   in do
        (vL, vM) <- (,) <$> varList <*> sequenceA varMap
        let (inL, outL) = partition ((== Input) . fst) $ filter ((/= Local) . fst) vL
        let update (_, vId) = (vId, vM Map.! vId)
        return
          ( NodeSignature
              { nodeArity = length inL,
                inputTypes = update <$> inL,
                outputTypes = NonEmpty.fromList $ update <$> outL,
                sizeVars = Map.toList $ sizeInfoVariables sInfo,
                sizeConstraints = sizeInfoContraints sInfo
              },
            VarMapping vM mempty
          )

addLocals :: SizeInfo -> VarMapping -> NodeVarEnv () -> CanFail VarMapping
addLocals sInfo VarMapping {topLevelVars} (NodeVarEnv m) =
  let initState = NodeVarState {varList = pure [], varMap = fmap pure topLevelVars}
      NodeVarState {varMap} = execState (runReaderT m sInfo) initState
   in do
        allVars <- sequenceA varMap
        let localVars = Map.difference allVars topLevelVars
        return $ VarMapping {topLevelVars, localVars}
