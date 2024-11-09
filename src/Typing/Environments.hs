{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Typing.Environments
  ( -- NodeEnv
    NodeEnv (),
    addNode,
    runNodeEnv,
    -- NodeVarEnv
    NodeVarEnv (),
    addInputVariable,
    addOutputVariable,
    addLocalVariable,
    -- ExprEnv
    ExprEnv (),
    findNode,
    findVariable,
    -- Bind Monad
    evalIn,
  )
where

import Data.Functor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Typing.Ast
import Typing.TypeError

-- # Section NodeEnv Monad

type NodeState = Map NodeIdent (Maybe NodeSignature)

type NodeWriter = CanFail [(NodeIdent, Localized TNode)]

newtype NodeEnv a = NodeEnv {runNodeEnv' :: NodeState -> (a, NodeState, NodeWriter)}

instance Functor NodeEnv where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> NodeEnv a -> NodeEnv b
  fmap f m = NodeEnv $ \s ->
    let (a, s', w) = runNodeEnv' m s in (f a, s', w)

instance Applicative NodeEnv where
  {-# INLINEABLE pure #-}
  pure :: a -> NodeEnv a
  pure x = NodeEnv (x,,mempty)

  {-# INLINEABLE (<*>) #-}
  (<*>) :: NodeEnv (a -> b) -> NodeEnv a -> NodeEnv b
  m1 <*> m2 = NodeEnv $ \s ->
    let (f, s', w) = runNodeEnv' m1 s
        (x, s'', w') = runNodeEnv' m2 s'
     in (f x, s'', w <> w')

instance Monad NodeEnv where
  {-# INLINEABLE (>>=) #-}
  (>>=) :: NodeEnv a -> (a -> NodeEnv b) -> NodeEnv b
  m >>= f = NodeEnv $ \s ->
    let (x, s', w) = runNodeEnv' m s
        (y, s'', w') = runNodeEnv' (f x) s'
     in (y, s'', w <> w')

checkNode :: Localized TNode -> CanFail (Localized TNode)
checkNode = pure -- TODO Check Node Here

addNode :: Localized a -> Localized Ident -> CanFail NodeContext -> CanFail [TEquation] -> NodeEnv ()
addNode loc name ctx eqs =
  let nodeName = unwrap name
      nodeId = NodeIdent nodeName
      tnode = collapse $ checkNode . (loc $>) <$> (TNode <$> ctx <*> eqs)
   in NodeEnv $ \s ->
        case Map.lookup nodeId s of
          Just _ -> ((), s, reportError name $ NodeAlreadyDeclared nodeName)
          Nothing -> ((), Map.insert nodeId nodeSig s, List.singleton . (,) nodeId <$> tnode)
  where
    nodeSig = withError Nothing (Just . buildSig) ctx
    buildSig n =
      let varMap = tnodeVarTypes n
          inputTyp = (!) varMap . unwrap <$> tnodeInput n
          outputTyp =
            case (!) varMap . unwrap <$> tnodeOutput n of
              x :| [] -> Atom x
              x :| (y : l) -> Tuple $ BiList (Atom x) (Atom y) (Atom <$> l)
       in NodeSignature (length inputTyp) inputTyp outputTyp

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv m =
  let ((), s, w) = runNodeEnv' m Map.empty
      s' = Map.mapMaybe id s -- Invariant when w has no error, s has no Maybe
   in TAst s' <$> w

-- # End Section

-- # Section NodeVarType Monad

data VarKind = Input | Output | Local

type VarMapping = Map VarIdent (Maybe AtomicType)

data NodeVarState = NodeVarState
  { vList :: CanFail [(VarKind, Localized VarIdent)],
    vMap :: VarMapping
  }

newtype NodeVarEnv a = NodeVarEnv {runNodeVarEnv' :: NodeVarState -> (a, NodeVarState)}

instance Functor NodeVarEnv where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> NodeVarEnv a -> NodeVarEnv b
  fmap f m = NodeVarEnv $ \s ->
    let (x, s') = runNodeVarEnv' m s in (f x, s')

instance Applicative NodeVarEnv where
  {-# INLINEABLE pure #-}
  pure :: a -> NodeVarEnv a
  pure x = NodeVarEnv (x,)

  {-# INLINEABLE (<*>) #-}
  (<*>) :: NodeVarEnv (a -> b) -> NodeVarEnv a -> NodeVarEnv b
  m1 <*> m2 = NodeVarEnv $ \s ->
    let (f, s') = runNodeVarEnv' m1 s
        (x, s'') = runNodeVarEnv' m2 s'
     in (f x, s'')

instance Monad NodeVarEnv where
  {-# INLINEABLE (>>=) #-}
  (>>=) :: NodeVarEnv a -> (a -> NodeVarEnv b) -> NodeVarEnv b
  m >>= f = NodeVarEnv $ \s ->
    let (x, s') = runNodeVarEnv' m s
     in runNodeVarEnv' (f x) s'

addVariable :: VarKind -> Localized Ident -> CanFail AtomicType -> NodeVarEnv ()
addVariable k v typ =
  let varName = unwrap v
      varTyp = withError Nothing Just typ
      locVarId = VarIdent <$> v
      varId = unwrap locVarId
   in NodeVarEnv $ \s ->
        case Map.lookup varId (vMap s) of
          -- Don't forget to carry the potential error coming from typ !
          Just _ ->
            ((), s {vList = addToList s $ addError typ v (VariableAlreadyDeclared varName)})
          Nothing ->
            ( (),
              NodeVarState
                { vList = addToList s $ typ $> [(k, locVarId)],
                  vMap = Map.insert varId varTyp (vMap s)
                }
            )
  where
    addToList s x = (<>) <$> vList s <*> x

addInputVariable :: Localized Ident -> CanFail AtomicType -> NodeVarEnv ()
addInputVariable = addVariable Input

addOutputVariable :: Localized Ident -> CanFail AtomicType -> NodeVarEnv ()
addOutputVariable = addVariable Output

addLocalVariable :: Localized Ident -> CanFail AtomicType -> NodeVarEnv ()
addLocalVariable = addVariable Local

-- # End Section

-- # Section ExprEnv Monad

newtype ExprEnv a = ExprEnv {runExprEnv' :: NodeState -> VarMapping -> a}

instance Functor ExprEnv where
  fmap :: (a -> b) -> ExprEnv a -> ExprEnv b
  fmap f m = ExprEnv $ \ns vs -> f $ runExprEnv' m ns vs

instance Applicative ExprEnv where
  pure :: a -> ExprEnv a
  pure x = ExprEnv $ \_ _ -> x

  (<*>) :: ExprEnv (a -> b) -> ExprEnv a -> ExprEnv b
  m1 <*> m2 = ExprEnv $ \ns vs -> runExprEnv' m1 ns vs $ runExprEnv' m2 ns vs

instance Monad ExprEnv where
  (>>=) :: ExprEnv a -> (a -> ExprEnv b) -> ExprEnv b
  m >>= f = ExprEnv $ \ns vs -> runExprEnv' (f $ runExprEnv' m ns vs) ns vs

findNode :: Localized Ident -> ExprEnv (CanFail (Localized NodeIdent, NodeSignature))
findNode nodeName = ExprEnv $ \ns _ ->
  maybe (reportError nodeName $ UnknownNode nodeIdent) nodeFound $ Map.lookup nodeId ns
  where
    locNodeId = NodeIdent <$> nodeName
    nodeIdent = unwrap nodeName
    nodeId = unwrap locNodeId

    nodeFound Nothing = hasFailed -- Node declared but error in the type declaration
    nodeFound (Just sig) = pure (locNodeId, sig)

findVariable :: Localized Ident -> ExprEnv (CanFail (Localized VarIdent, AtomicType))
findVariable varName =
  ExprEnv $ \_ vs ->
    maybe (reportError varName $ UnknownVariable varIdent) varFound $ Map.lookup varId vs
  where
    locVarId = VarIdent <$> varName
    varIdent = unwrap varName
    varId = unwrap locVarId

    varFound Nothing = hasFailed -- Variable declared but error in the type declaration
    varFound (Just typ) = pure (locVarId, typ)

evalIn :: NodeVarEnv () -> ExprEnv a -> NodeEnv (CanFail NodeContext, a)
evalIn env expr = NodeEnv $ \ns -> ((buildCtx <$> vList varEnv, runExprEnv' expr ns (vMap varEnv)), ns, mempty)
  where
    varEnv = snd $ runNodeVarEnv' env NodeVarState {vList = pure [], vMap = Map.empty}
    varMap = Map.mapMaybe id $ vMap varEnv -- Invariant : When no error is in vList, varMap has no Nothing
    buildCtx l =
      let (inL, outL, locL) = foldl f ([], [], []) l
       in NodeContext inL (NonEmpty.fromList outL) locL varMap

    f (inL, outL, locL) (Input, v) = (inL <> [v], outL, locL)
    f (inL, outL, locL) (Output, v) = (inL, outL <> [v], locL)
    f (inL, outL, locL) (Local, v) = (inL, outL, locL <> [v])

-- # End Section
