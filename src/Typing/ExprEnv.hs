{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.ExprEnv
  ( ExprEnv (),
    unifToExpr,
    findNode,
    findVariable,
    buildIfCondEq,
    buildFbyEq,
    buildCallEq,
    runExprEnv,
  )
where

import Commons.Ast
import Commons.Ids
import Commons.Position
import Commons.Types
import Commons.TypingError
import Control.Monad (zipWithM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map.Lazy (Map, (!))
import Data.Set
import qualified Data.Set as Set
import Data.Text.Lazy (unpack)
import Typing.Ast
import Typing.Environments
import Typing.NodeEnv (NodeEnv, toNodeEnv)
import Typing.TypeUnification (TypeCand, TypeUnifier, runUnifier)

type TCandEq = TEquation TypeCand

findNode :: Pos Ident -> ExprEnv (CanFail (NodeIdent, NodeSignature))
findNode nodeName = ExprEnv $ asks (findNode' . fst)
  where
    nodeId = NodeIdent $ unwrap nodeName
    findNode' ns =
      case Map.lookup nodeId ns of
        Just sig -> (nodeId,) <$> sig
        Nothing ->
          let Ident s = unwrap nodeName
           in reportError nodeName $ "Unknown node " <> unpack s <> "."

findVariable :: Pos Ident -> ExprEnv (CanFail (VarIdent, AtomicTType))
findVariable varName = ExprEnv $ asks (findVariable' . snd)
  where
    varId = VarIdent varName
    findVariable' vs =
      case Map.lookup varId vs of
        Just sig -> (varId,) <$> sig
        Nothing ->
          let Ident s = unwrap varName
           in reportError varName $ "Unknown variable " <> unpack s <> "."

unifToExpr :: TypeUnifier a -> ExprEnv a
unifToExpr = ExprEnv . lift . lift

data VarInfo
  = WithType AtomicTType
  | DerivingFrom TypeCand

data ExprState = ExprState
  { sideEqs :: [TCandEq],
    varsInfo :: Map VarId VarInfo,
    nextVarId :: Int
  }

newtype ExprEnv a = ExprEnv (ReaderT (NodeMapping, VarMapping) (StateT ExprState TypeUnifier) a)

instance Functor ExprEnv where
  fmap :: (a -> b) -> ExprEnv a -> ExprEnv b
  fmap f (ExprEnv m) = ExprEnv $ f <$> m

instance Applicative ExprEnv where
  pure :: a -> ExprEnv a
  pure = ExprEnv . pure

  (<*>) :: ExprEnv (a -> b) -> ExprEnv a -> ExprEnv b
  (<*>) (ExprEnv f) (ExprEnv arg) = ExprEnv $ f <*> arg

instance Monad ExprEnv where
  (>>=) :: ExprEnv a -> (a -> ExprEnv b) -> ExprEnv b
  (>>=) (ExprEnv m) f = ExprEnv $ m >>= unwrapM . f
    where
      unwrapM (ExprEnv x) = x

freshVar :: (Int -> VarId) -> VarInfo -> ExprEnv VarId
freshVar gVar vInfo = ExprEnv . state $ freshVar' gVar vInfo
  where
    freshVar' genVar info ExprState {sideEqs, varsInfo, nextVarId} =
      let v = genVar nextVarId
          newVarsInfo = Map.insert v info varsInfo
          newNextId = nextVarId + 1
       in (v, ExprState sideEqs newVarsInfo newNextId)

addEq :: TCandEq -> ExprEnv ()
addEq eq = ExprEnv . modify $ \s -> s {sideEqs = eq : sideEqs s}

buildIfCondEq :: (VarIdent, TExpr TypeCand, TypeCand) -> ExprEnv VarId
buildIfCondEq (varOrig, expr, _) = do
  vId <- freshVar (VarIfCondition varOrig) (WithType TBool)
  () <- addEq (SimpleTEq vId expr)
  return vId

buildFbyEq :: VarIdent -> TExpr TypeCand -> TExpr TypeCand -> TypeCand -> ExprEnv VarId
buildFbyEq varOrig initFby nextFby typeCand = do
  vId <- freshVar (VarFbyDefinition varOrig) (DerivingFrom typeCand)
  () <- addEq (FbyTEq vId initFby nextFby)
  return vId

buildCallEq :: (NodeIdent, NodeSignature) -> [TExpr TypeCand] -> ExprEnv (NonEmpty (VarId, AtomicTType))
buildCallEq (nodeId, nodeSig) args = do
  outVarsIds <- mapM freshOutVarFromTyp (outputType nodeSig)
  inVarsIds <- fmap fst <$> mapM freshInVarFromTyp (inputTypes nodeSig)
  _ <- zipWithM_ (\v e -> addEq $ SimpleTEq v e) inVarsIds args
  () <- addEq (CallTEq (fst <$> outVarsIds) nodeId inVarsIds)
  return outVarsIds
  where
    freshOutVarFromTyp t = freshVar (OutputCallVar nodeId) (WithType t) <&> (,t)
    freshInVarFromTyp (_, t) = freshVar (InputCallVar nodeId) (WithType t) <&> (,t)

runExprEnv :: NodeEnvironment VarIdent -> ExprEnv (CanFail (NonEmpty TCandEq)) -> NodeEnv (CanFail TNode)
runExprEnv nodeEnv m = toNodeEnv (runExprEnv' nodeEnv m)

runExprEnv' :: NodeEnvironment VarIdent -> ExprEnv (CanFail (NonEmpty TCandEq)) -> NodeMapping -> CanFail TNode
runExprEnv' nodeEnv (ExprEnv m) ns = Node <$> newNodeEnv <*> sanitizedEqs
  where
    m' = runReaderT m (ns, vMapping nodeEnv)
    m'' = runStateT m' (ExprState {sideEqs = [], varsInfo = Map.empty, nextVarId = 0})
    ((tEqs, s), typeS) = runUnifier m''
    sanitizedEqs = fmap fmap (sanitizeEq <$> typeS) <*> (NonEmpty.prependList (sideEqs s) <$> tEqs)
    newNodeEnv = buildNewEnv <$> typeS <*> nCtx nodeEnv

    buildNewEnv typSt nCtx =
      let (newLocals, newVarTypes) = Map.foldMapWithKey (fromFreshVars typSt) (varsInfo s)
       in newContext nCtx FromIdent newLocals newVarTypes

    fromFreshVars :: Map TypeCand AtomicTType -> VarId -> VarInfo -> (Set VarId, Map VarId AtomicTType)
    fromFreshVars _ vId (WithType aTyp) = (Set.singleton vId, Map.singleton vId aTyp)
    fromFreshVars typSt vId (DerivingFrom tCand) = (Set.singleton vId, Map.singleton vId (typSt ! tCand))

    sanitizeEq typSt eq = fmap (typSt !) eq
