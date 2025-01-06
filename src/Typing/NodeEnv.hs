{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Typing.NodeEnv
  ( NodeEnv (),
    addNode,
    runNodeEnv,
    toNodeEnv,
  )
where

import Commons.Ast
import Commons.Ids
import Commons.Position
import Commons.TypingError
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Foldable
import Data.Functor
import qualified Data.List as List
import Data.Map (Map)
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Typing.Ast
import Typing.Environments

type NodeWriter = CanFail [(NodeIdent, TNode)]

newtype NodeEnv a = NodeEnv (StateT NodeMapping (Writer NodeWriter) a)

instance Functor NodeEnv where
  fmap :: (a -> b) -> NodeEnv a -> NodeEnv b
  fmap f (NodeEnv m) = NodeEnv $ f <$> m

instance Applicative NodeEnv where
  pure :: a -> NodeEnv a
  pure = NodeEnv . pure

  (<*>) :: NodeEnv (a -> b) -> NodeEnv a -> NodeEnv b
  (<*>) (NodeEnv f) (NodeEnv arg) = NodeEnv $ f <*> arg

instance Monad NodeEnv where
  (>>=) :: NodeEnv a -> (a -> NodeEnv b) -> NodeEnv b
  (>>=) (NodeEnv m) f = NodeEnv $ m >>= unwrapM . f
    where
      unwrapM (NodeEnv x) = x

addNode :: Pos Ident -> CanFail TNode -> NodeEnv ()
addNode nName nNode =
  let nodeId = NodeIdent nName
   in do
        nMap <- NodeEnv get
        case Map.lookup nodeId nMap of
          Just _ -> writeError $ "Node " <> show nName <> " is already declared."
          Nothing -> addNodeMapping nodeId nMap >> writeNode nodeId
  where
    writeError err = NodeEnv . tell $ addError nNode nName err
    addNodeMapping nId nMap = NodeEnv . put $ Map.insert nId (buildSig <$> nNode) nMap
    writeNode nId =
      let vNode = collapse $ validateNode nName <$> nNode
       in NodeEnv . tell $ List.singleton . (,) nId <$> vNode

    buildSig :: TNode -> NodeSignature
    buildSig (Node n _) =
      let varTyp = nodeVarTypes n
          buildInput varIn = (varIn, varTyp ! FromIdent varIn)
          inputTyp = buildInput <$> nodeInput n
          outputTyp = (!) varTyp . FromIdent <$> nodeOutput n
       in NodeSignature (length inputTyp) inputTyp outputTyp

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv (NodeEnv m) =
  let (((), s), w) = runWriter $ runStateT m Map.empty in Ast <$> sequenceA s <*> w

toNodeEnv :: (NodeMapping -> a) -> NodeEnv a
toNodeEnv = NodeEnv . gets

validateNode :: Pos a -> TNode -> CanFail TNode
validateNode loc node =
  let defError = checkVarDef loc node
      finalErr = collapse $ const (checkCausality loc node) <$> defError
   in finalErr $> node

varError :: Pos a -> String -> (String -> String) -> VarId -> CanFail b
varError _ _ f (FromIdent var@(VarIdent vLoc)) = reportError vLoc . f $ show var
varError loc errId _ var = reportError loc $ "Unknown Typing error (" <> errId <> " " <> show var <> ")"

declVars :: NodeContext VarId -> Set VarId
declVars NodeContext {nodeInput, nodeOutput, nodeLocal} =
  let inSet = Set.fromList $ FromIdent <$> nodeInput
      outSet = Set.fromList $ FromIdent <$> toList nodeOutput
   in inSet `Set.union` outSet `Set.union` nodeLocal

checkVarDef :: Pos a -> TNode -> CanFail ()
checkVarDef loc (Node nCtx eqs) =
  let definedVars = foldMap defVar eqs
      declaredVars = declVars nCtx
      noDefVar = Set.difference declaredVars definedVars
   in if Set.null noDefVar
        then pure ()
        else foldMap undefError noDefVar
  where
    defVar (SimpleEq x _) = Set.singleton x
    defVar (FbyEq x _ _) = Set.singleton x
    defVar (CallEq l _ _) = Set.fromList $ toList l

    undefError = varError loc "Undef" $ \x -> "The variable " <> x <> " is not defined."

freeVars :: TExpr a -> Set VarId
freeVars = freeVars' . unwrap
  where
    freeVars' (ConstantTExpr _ _) = Set.empty
    freeVars' (VarTExpr v _) = Set.singleton v
    freeVars' (UnOpTExpr _ arg _) = freeVars arg
    freeVars' (BinOpTExpr _ lhs rhs _) = freeVars lhs `Set.union` freeVars rhs
    freeVars' (IfTExpr cond tb fb _) = Set.singleton cond `Set.union` freeVars tb `Set.union` freeVars fb

type CausalityGraph = Map VarId (Set VarId)

data VarStatus = InProcess | Done

data DFSState = DFSState {stack :: [VarId], varStatus :: Map VarId VarStatus}

data DFSError = DFSError {var :: VarId, varCycle :: [VarId]}

type DFS = ReaderT CausalityGraph (StateT DFSState (Except DFSError))

checkCausality :: Pos a -> TNode -> CanFail ()
checkCausality loc node@(Node nCtx _) =
  let g = buildCausalityGraph node
      s = DFSState {stack = mempty, varStatus = mempty}
      res = runExcept (evalStateT (runReaderT doDFS g) s)
   in case res of
        Left err -> causalityError err
        Right () -> pure ()
  where
    doDFS = mapM_ dfs $ FromIdent <$> nodeOutput nCtx
    causalityError DFSError {var, varCycle} =
      varError
        loc
        "CausalityError"
        (\x -> "This use of the variable " <> x <> " form a cycle: " <> show varCycle <> ".")
        var

buildCausalityGraph :: TNode -> CausalityGraph
buildCausalityGraph (Node _ eqs) =
  foldMap go eqs
  where
    go (SimpleEq v expr) = Map.singleton v (freeVars expr)
    go (FbyEq x initE _) = Map.singleton x (freeVars initE)
    go (CallEq l _ args) =
      let argsDeps = foldMap freeVars args
       in Map.fromList $ (,argsDeps) <$> toList l

dfs :: VarId -> DFS ()
dfs v = do
  vState <- gets $ Map.lookup v . varStatus
  case vState of
    Nothing ->
      pushToStack v
        >> markAs InProcess
        >> asks (maybe Set.empty id . Map.lookup v)
        >>= mapM_ dfs
        >> markAs Done
        >> popFromStack
    Just InProcess -> gets stack >>= throwError . DFSError v
    Just Done -> return ()
  where
    pushToStack :: VarId -> DFS ()
    pushToStack x = modify $ \s -> s {stack = x : (stack s)}

    popFromStack :: DFS ()
    popFromStack = modify $ \s -> s {stack = drop 1 (stack s)}

    markAs :: VarStatus -> DFS ()
    markAs x = modify $ \s -> s {varStatus = Map.insert v x (varStatus s)}
