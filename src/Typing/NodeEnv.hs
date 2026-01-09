{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Typing.NodeEnv
  ( NodeEnv (),
    NodeMapping,
    registerNode,
    runNodeEnv,
    toNodeEnv,
  )
where

import Commons.Ast (Ast (..), Body, Node (..), NodeBody (..), NodeSignature (..))
import Commons.Error (CanFail, addError, collapse, reportError)
import Commons.Ids (Ident, NodeIdent (..), VarId (..), VarIdent (..))
import Commons.Position (Pos (..))
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, execStateT, gets, modify)
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Either (rights)
import Data.Foldable (Foldable (toList))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Typing.Ast (TAst, TBody, TEquation (..), TExpr (..), TNode)

type NodeWriter = CanFail [(NodeIdent, TNode)]

type NodeMapping = Map NodeIdent (CanFail NodeSignature)

newtype NodeEnv a
  = NodeEnv
      ( StateT
          (Map NodeIdent (CanFail NodeSignature, Pos Ident))
          (Writer NodeWriter)
          a
      )
  deriving (Functor, Applicative, Monad)

registerNode :: Pos Ident -> CanFail NodeSignature -> NodeEnv (CanFail NodeIdent, CanFail (Body TBody) -> NodeEnv ())
registerNode nName nSig =
  let nodeId = NodeIdent $ unwrap nName
   in NodeEnv . state $ \nMap ->
        case Map.lookup nodeId nMap of
          Just _ ->
            let err = addError nSig nName $ "Node " <> show nName <> " is already declared."
             in ((err, const $ return ()), nMap)
          Nothing -> do
            -- We register the node
            ((pure nodeId, addNode nodeId), Map.insert nodeId (nSig, nName) nMap)
  where
    addNode :: NodeIdent -> CanFail (Body TBody) -> NodeEnv ()
    addNode nId nBody = do
      let newN = Node <$> nSig <*> nBody
      let vNode = collapse $ validateNode nName <$> newN
      NodeEnv $ tell $ List.singleton . (nId,) <$> vNode

toNodeEnv :: (NodeMapping -> a) -> NodeEnv a
toNodeEnv f = NodeEnv . gets $ f . fmap fst

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv (NodeEnv m) =
  let (s, w) = runWriter $ execStateT m Map.empty
   in Ast <$> traverse fst s <*> w

validateNode :: Pos a -> TNode -> CanFail TNode
validateNode loc node =
  const node <$ checkVarDef loc node <*> checkCausality loc node

varError :: Pos a -> String -> (String -> String) -> VarId -> CanFail b
varError _ _ f (FromIdent var@(VarIdent vLoc)) = reportError vLoc . f $ show var
varError loc errId _ var = reportError loc $ "Unknown Typing error (" <> errId <> " " <> show var <> ")"

checkVarDef :: Pos a -> TNode -> CanFail ()
checkVarDef loc (Node nSig nBody) =
  foldMap (checkVarDefInBody loc nSig) nBody

checkVarDefInBody :: Pos a -> NodeSignature -> TBody -> CanFail ()
checkVarDefInBody loc nSig (NodeBody locVars eqs) =
  let definedVars = foldMap defVar eqs <> Set.fromList (FromIdent . fst <$> inputTypes nSig)
      declaredVars = Map.keysSet locVars
      noDefVar = Set.difference declaredVars definedVars
   in if Set.null noDefVar
        then pure ()
        else foldMap undefError noDefVar
  where
    defVar (SimpleTEq x _) = Set.singleton x
    defVar (FbyTEq x _ _) = Set.singleton x
    defVar (CallTEq l _ _ _) = Set.fromList $ toList l

    undefError = varError loc "Undef" $ \x -> "The variable " <> x <> " is not defined."

freeVars :: TExpr a -> Set VarId
freeVars = freeVars'
  where
    freeVars' (ConstantTExpr _ _) = Set.empty
    freeVars' (VarTExpr v _) = Set.singleton v
    freeVars' (UnOpTExpr _ arg _) = freeVars arg
    freeVars' (BinOpTExpr _ lhs rhs _) = freeVars lhs `Set.union` freeVars rhs
    freeVars' (IfTExpr cond tb fb _) = Set.singleton cond `Set.union` freeVars tb `Set.union` freeVars fb
    freeVars' (ConcatTExpr lhs rhs _) = freeVars lhs `Set.union` freeVars rhs
    freeVars' (SliceTExpr arg _ _) = freeVars arg
    freeVars' (SelectTExpr arg _ _) = freeVars arg
    freeVars' (ConvertTExpr arg _) = freeVars arg

type CausalityGraph = Map VarId (Set VarId)

data VarStatus = InProcess | Done

data DFSState = DFSState {stack :: [VarId], varStatus :: Map VarId VarStatus}

data DFSError = DFSError {var :: VarId, varCycle :: [VarId]}

type DFS = ReaderT CausalityGraph (StateT DFSState (Except DFSError))

checkCausality :: Pos a -> TNode -> CanFail ()
checkCausality loc (Node nSig bodies) =
  foldMap checkBody bodies
  where
    checkBody b =
      let g = buildCausalityGraph b
          s = DFSState {stack = mempty, varStatus = mempty}
          res = runExcept (evalStateT (runReaderT doDFS g) s)
       in case res of
            Left err -> causalityError err
            Right () -> pure ()

    doDFS = mapM_ dfs $ FromIdent . fst <$> outputTypes nSig
    causalityError DFSError {var, varCycle} =
      varError
        loc
        "CausalityError"
        (\x -> "This use of the variable " <> x <> " form a cycle: " <> show varCycle <> ".")
        var

buildCausalityGraph :: TBody -> CausalityGraph
buildCausalityGraph (NodeBody {bodyEqs}) =
  foldMap go bodyEqs
  where
    go (SimpleTEq v expr) = Map.singleton v (freeVars expr)
    go (FbyTEq x initE _) = Map.singleton x (freeVars initE)
    go (CallTEq l _ _ args) =
      let argsDeps = Set.fromList $ rights args
       in Map.fromList $ (,argsDeps) <$> toList l

dfs :: VarId -> DFS ()
dfs v = do
  vState <- gets $ Map.lookup v . varStatus
  case vState of
    Nothing ->
      pushToStack v
        >> markAs InProcess
        >> asks (fromMaybe Set.empty . Map.lookup v)
        >>= mapM_ dfs
        >> markAs Done
        >> popFromStack
    Just InProcess -> gets stack >>= throwError . DFSError v
    Just Done -> return ()
  where
    pushToStack :: VarId -> DFS ()
    pushToStack x = modify $ \s -> s {stack = x : stack s}

    popFromStack :: DFS ()
    popFromStack = modify $ \s -> s {stack = drop 1 (stack s)}

    markAs :: VarStatus -> DFS ()
    markAs x = modify $ \s -> s {varStatus = Map.insert v x (varStatus s)}
