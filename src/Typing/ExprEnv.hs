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
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as Set
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
    nodeId = NodeIdent nodeName
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
  () <- addEq (SimpleEq vId expr)
  return vId

buildFbyEq :: VarIdent -> TExpr TypeCand -> TExpr TypeCand -> TypeCand -> ExprEnv VarId
buildFbyEq varOrig initFby nextFby typeCand = do
  vId <- freshVar (VarFbyDefinition varOrig) (DerivingFrom typeCand)
  () <- addEq (FbyEq vId initFby nextFby)
  return vId

buildCallEq :: (NodeIdent, NodeSignature) -> [TExpr TypeCand] -> ExprEnv (NonEmpty (VarId, AtomicTType))
buildCallEq (nodeId, nodeSig) args = do
  varIds <- mapM freshVarFromTyp (outputType nodeSig)
  () <- addEq (CallEq (fst <$> varIds) nodeId args)
  return varIds
  where
    freshVarFromTyp t = freshVar (OutputCallVar nodeId) (WithType t) <&> (,t)

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
          oldLocals = Set.foldMap (Set.singleton . FromIdent) (nodeLocal nCtx)
          convertOldBinding varIdent aTyp = Map.singleton (FromIdent varIdent) aTyp
          oldVarTypes = Map.foldMapWithKey convertOldBinding (nodeVarTypes nCtx)
       in nCtx {nodeLocal = oldLocals <> newLocals, nodeVarTypes = oldVarTypes <> newVarTypes}

    fromFreshVars :: Map TypeCand AtomicTType -> VarId -> VarInfo -> (Set VarId, Map VarId AtomicTType)
    fromFreshVars _ vId (WithType aTyp) = (Set.singleton vId, Map.singleton vId aTyp)
    fromFreshVars typSt vId (DerivingFrom tCand) = (Set.singleton vId, Map.singleton vId (typSt ! tCand))

    sanitizeEq typSt (SimpleEq vId e) = SimpleEq vId $ sExpr typSt e
    sanitizeEq typSt (FbyEq vId initFby nextFby) = FbyEq vId (sExpr typSt initFby) $ sExpr typSt nextFby
    sanitizeEq typSt (CallEq vIds nId args) = CallEq vIds nId $ sExpr typSt <$> args

    sExpr typSt = fmap (sExprDesc typSt)

    sExprDesc :: Map TypeCand AtomicTType -> TExprKind TypeCand -> TExprKind AtomicTType
    sExprDesc typSt (ConstantTExpr c aT) = ConstantTExpr c $ typSt ! aT
    sExprDesc typSt (VarTExpr vId aT) = VarTExpr vId $ typSt ! aT
    sExprDesc typSt (UnOpTExpr op e aT) = UnOpTExpr op (sExpr typSt e) $ typSt ! aT
    sExprDesc typSt (BinOpTExpr op lhs rhs aT) = BinOpTExpr op (sExpr typSt lhs) (sExpr typSt rhs) $ typSt ! aT
    sExprDesc typSt (IfTExpr ifVar ifTrue ifFalse ifTyp) =
      IfTExpr ifVar (sExpr typSt ifTrue) (sExpr typSt ifFalse) $ typSt ! ifTyp
