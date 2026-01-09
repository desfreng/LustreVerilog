{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Typing.ExprEnv
  ( ExprEnv (),
    unifToExpr,
    findNode,
    findVariable,
    isCurrentNode,
    buildIfCondEq,
    buildFbyEq,
    buildCallEq,
    runExprEnv,
  )
where

import Commons.Ast (Constant, NodeSignature)
import Commons.Error (CanFail, reportError)
import Commons.Ids (Ident (..), NodeIdent (..), VarId (..), VarIdent (..))
import Commons.Position (Pos (..))
import Commons.Size (Size)
import Commons.Types (AtomicTType (TBool))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT (runStateT), lift, modify, state)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map.Lazy (Map, (!))
import Data.Text (unpack)
import Typing.Ast (TEquation (..), TExpr (ConstantTExpr, VarTExpr))
import Typing.NodeEnv (NodeEnv, NodeMapping, toNodeEnv)
import Typing.NodeVarEnv (VarMapping)
import qualified Typing.SizeEnv as S
import Typing.TypeUnification (TypeCand, TypeUnifier, runUnifier)

type TCandEq = TEquation TypeCand

findNode :: Pos Ident -> ExprEnv (CanFail (NodeIdent, NodeSignature))
findNode nodeName = ExprEnv $ asks (findNode' . \(_, x, _) -> x)
  where
    nodeId = NodeIdent $ unwrap nodeName

    findNode' :: Map NodeIdent (CanFail a) -> CanFail (NodeIdent, a)
    findNode' ns =
      case Map.lookup nodeId ns of
        Just sig -> (nodeId,) <$> sig
        Nothing ->
          let Ident s = unwrap nodeName
           in reportError nodeName $ "Unknown node " <> unpack s <> "."

findVariable :: Pos Ident -> ExprEnv (CanFail (VarIdent, AtomicTType))
findVariable varName = ExprEnv $ asks (findVariable' . \(_, _, x) -> x)
  where
    varId = VarIdent varName
    findVariable' vs =
      case Map.lookup varId vs of
        Just sig -> pure (varId, sig)
        Nothing ->
          let Ident s = unwrap varName
           in reportError varName $ "Unknown variable " <> unpack s <> "."

isCurrentNode :: NodeIdent -> ExprEnv Bool
isCurrentNode nId = ExprEnv $ asks $ \(n, _, _) -> n == nId

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

newtype ExprEnv a = ExprEnv (ReaderT (NodeIdent, NodeMapping, VarMapping) (StateT ExprState TypeUnifier) a)
  deriving (Functor, Applicative, Monad)

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

buildCallEq :: NodeIdent -> [Size] -> [(TExpr TypeCand, AtomicTType)] -> NonEmpty AtomicTType -> ExprEnv (NonEmpty (VarId, AtomicTType))
buildCallEq nodeId sizeVarsArgs args outTypes = do
  outVarsIds <- mapM freshOutVarFromTyp outTypes
  inVarsArgs <- mapM freshInVarFromTyp args
  () <- addEq (CallTEq (fst <$> outVarsIds) nodeId sizeVarsArgs inVarsArgs)
  return outVarsIds
  where
    freshOutVarFromTyp :: AtomicTType -> ExprEnv (VarId, AtomicTType)
    freshOutVarFromTyp t = freshVar (OutputCallVar nodeId) (WithType t) <&> (,t)

    freshInVarFromTyp :: (TExpr TypeCand, AtomicTType) -> ExprEnv (Either (Constant, TypeCand) VarId)
    freshInVarFromTyp (ConstantTExpr cst typ, _) = return $ Left (cst, typ)
    freshInVarFromTyp (VarTExpr var _, _) = return $ Right var
    freshInVarFromTyp (e, t) = do
      newVar <- freshVar (InputCallVar nodeId) (WithType t)
      () <- addEq $ SimpleTEq newVar e
      return $ Right newVar

runExprEnv :: ExprEnv (CanFail (NonEmpty TCandEq)) -> NodeIdent -> VarMapping -> S.SizeInfo -> NodeEnv (CanFail (Map VarId AtomicTType, NonEmpty (TEquation AtomicTType)))
runExprEnv m nId vMapping sInfo = toNodeEnv $ runExprEnv' m nId vMapping sInfo

runExprEnv' :: ExprEnv (CanFail (NonEmpty TCandEq)) -> NodeIdent -> VarMapping -> S.SizeInfo -> NodeMapping -> CanFail (Map VarId AtomicTType, NonEmpty (TEquation AtomicTType))
runExprEnv' (ExprEnv m) nId vMapping sInfo ns =
  let m' = runReaderT m (nId, ns, vMapping)
      m'' = runStateT m' (ExprState {sideEqs = [], varsInfo = Map.empty, nextVarId = 0})
      ((tEqsF, s), typeSF) = runUnifier sInfo m''
   in do
        (tEqs, typeS) <- (,) <$> tEqsF <*> typeSF
        let sEq = fmap (typeS !) <$> NonEmpty.appendList tEqs (sideEqs s)
        let locEnv = Map.foldMapWithKey (fromFreshVars typeS) (varsInfo s)
        return (locEnv, sEq)
  where
    fromFreshVars :: Map TypeCand AtomicTType -> VarId -> VarInfo -> Map VarId AtomicTType
    fromFreshVars _ vId (WithType aTyp) = Map.singleton vId aTyp
    fromFreshVars typSt vId (DerivingFrom tCand) = Map.singleton vId (typSt ! tCand)
