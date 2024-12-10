{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Typing.Environments
  ( -- NodeEnv
    NodeEnv (),
    addNode,
    runNodeEnv,
    -- NodeVarEnv
    NodeEnvironment (),
    NodeVarEnv (),
    addInputVariable,
    addOutputVariable,
    addLocalVariable,
    runNodeVarEnv,
    -- ExprEnv
    ExprEnv (),
    findNode,
    findVariable,
    typeCheckIn,
    -- Unifying functions
    TypeUnifier (),
    TypeCand (),
    constantTypeCand,
    tupleTypeCand,
    fromAtomicType,
    nodeOutputTypeCand,
    unifyTypeCand,
    unifyWithTType,
    ExpectedAtom (),
    Unif.expectBitVector,
    Unif.expectBool,
    checkExpected,
  )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Functor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Data.String (IsString (..))
import Data.Text.Lazy (unpack)
import Typing.Ast
import Typing.Ids
import Typing.TypeError
import Typing.Unification (ExpectedAtom, TypeCand, TypeUnifier)
import qualified Typing.Unification as Unif

-- # Section NodeEnv Monad

type NodeState = Map NodeIdent (CanFail NodeSignature)

type NodeWriter = CanFail [(NodeIdent, Localized TNode)]

newtype NodeEnv a = NodeEnv (StateT NodeState (Writer NodeWriter) a)
  deriving (Functor, Applicative, Monad)

checkNode :: Localized TNode -> CanFail (Localized TNode)
checkNode = pure -- TODO Check Node Here

addNode :: Localized a -> Localized Ident -> NodeEnvironment -> CanFail [TEquation TType] -> NodeEnv ()
addNode loc name (NodeEnvironment {nCtx}) eqs =
  let nodeName@(Ident n) = unwrap name
      nodeId = NodeIdent nodeName
      tnode = collapse $ checkNode . (loc $>) <$> (TNode <$> nCtx <*> eqs)
   in NodeEnv $ do
        s <- get
        case Map.lookup nodeId s of
          Just _ ->
            tell (reportError name ("Variable " <> unpack n <> " is already declared."))
          Nothing ->
            put (Map.insert nodeId (buildSig <$> nCtx) s)
              >> tell (List.singleton . (,) nodeId <$> tnode)
  where
    buildSig n =
      let varTyp = tnodeVarTypes n
          inputTyp = (!) varTyp . unwrap <$> tnodeInput n
          outputTyp =
            case (!) varTyp . unwrap <$> tnodeOutput n of
              x :| [] -> TAtom x
              x :| (y : l) -> TTuple $ BiList (TAtom x) (TAtom y) (TAtom <$> l)
       in NodeSignature (length inputTyp) inputTyp outputTyp

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv (NodeEnv m) = let (((), s), w) = runWriter $ runStateT m Map.empty in TAst <$> sequenceA s <*> w

-- # End Section

-- # Section NodeVarType Monad

type VarMapping = Map VarIdent (CanFail AtomicTType)

data VarKind = Input | Output | Local

data NodeVarState = NodeVarState {varList :: CanFail [(VarKind, Localized VarIdent)], varMap :: VarMapping}

data NodeEnvironment = NodeEnvironment {nCtx :: CanFail NodeContext, vMapping :: VarMapping}

newtype NodeVarEnv a = NodeVarEnv (State NodeVarState a)
  deriving (Functor, Applicative, Monad)

addVariable :: VarKind -> Localized Ident -> CanFail AtomicTType -> NodeVarEnv ()
addVariable k v typ = NodeVarEnv $ modify addVariable'
  where
    Ident varName = unwrap v
    locVarId = VarIdent <$> v
    varId = unwrap locVarId

    addVariable' s = case Map.lookup varId (varMap s) of
      -- Don't forget to carry the potential error coming from typ !
      Just _ ->
        s {varList = (<>) <$> varList s <*> addError typ v ("Variable " <> unpack varName <> " is already declared.")}
      Nothing ->
        s
          { varList = (<>) <$> varList s <*> pure [(k, locVarId)],
            varMap = Map.insert varId typ (varMap s)
          }

addInputVariable :: Localized Ident -> CanFail AtomicTType -> NodeVarEnv ()
addInputVariable = addVariable Input

addOutputVariable :: Localized Ident -> CanFail AtomicTType -> NodeVarEnv ()
addOutputVariable = addVariable Output

addLocalVariable :: Localized Ident -> CanFail AtomicTType -> NodeVarEnv ()
addLocalVariable = addVariable Local

runNodeVarEnv :: NodeVarEnv () -> NodeEnv NodeEnvironment
runNodeVarEnv (NodeVarEnv m) =
  let s = execState m NodeVarState {varList = pure [], varMap = Map.empty}
      vL = filterList <$> varList s
      vM = sequenceA (varMap s)
   in NodeEnv . state $ (NodeEnvironment (buildCtx <$> vL <*> vM) (varMap s),)
  where
    filterList l =
      let (inL, outL, locL) = foldl f ([], [], []) l
       in (inL, NonEmpty.fromList outL, locL)

    buildCtx (inL, outL, locL) = NodeContext inL outL locL

    f (inL, outL, locL) (Input, v) = (inL <> [v], outL, locL)
    f (inL, outL, locL) (Output, v) = (inL, outL <> [v], locL)
    f (inL, outL, locL) (Local, v) = (inL, outL, locL <> [v])

-- # End Section

-- # Section ExprEnv Monad

newtype ExprEnv a = ExprEnv (ReaderT (NodeState, VarMapping) TypeUnifier a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (ExprEnv a) where
  (<>) :: ExprEnv a -> ExprEnv a -> ExprEnv a
  (<>) = liftA2 (<>)

instance IsString (ExprEnv String) where
  fromString :: String -> ExprEnv String
  fromString = pure

findNode :: Localized Ident -> ExprEnv (CanFail (Localized NodeIdent, NodeSignature))
findNode nodeName = ExprEnv $ asks (findNode' . fst)
  where
    locNodeId = NodeIdent <$> nodeName
    findNode' ns =
      case Map.lookup (unwrap locNodeId) ns of
        Just sig -> (locNodeId,) <$> sig
        Nothing ->
          let Ident s = unwrap nodeName
           in reportError nodeName $ "Unknown node " <> unpack s <> "."

findVariable :: Localized Ident -> ExprEnv (CanFail (Localized VarIdent, AtomicTType))
findVariable varName = ExprEnv $ asks (findVariable' . snd)
  where
    locVarId = VarIdent <$> varName
    findVariable' vs =
      case Map.lookup (unwrap locVarId) vs of
        Just sig -> (locVarId,) <$> sig
        Nothing ->
          let Ident s = unwrap varName
           in reportError varName $ "Unknown variable " <> unpack s <> "."

constantTypeCand :: Constant -> ExprEnv TypeCand
constantTypeCand c = ExprEnv . lift $ Unif.constantTypeCand c

tupleTypeCand :: BiList TypeCand -> ExprEnv TypeCand
tupleTypeCand l = ExprEnv . lift $ Unif.tupleTypeCand l

fromAtomicType :: AtomicTType -> ExprEnv TypeCand
fromAtomicType atom = ExprEnv . lift $ Unif.fromAtomicType atom

nodeOutputTypeCand :: NodeSignature -> ExprEnv TypeCand
nodeOutputTypeCand sig = ExprEnv . lift $ Unif.nodeOutputTypeCand sig

unifyTypeCand :: ErrorReporter -> TypeCand -> TypeCand -> ExprEnv (CanFail TypeCand)
unifyTypeCand err t1 t2 = ExprEnv . lift $ Unif.unifyTypeCand err t1 t2

unifyWithTType :: ErrorReporter -> TType -> TypeCand -> ExprEnv (CanFail TypeCand)
unifyWithTType err typ typeCand = ExprEnv . lift $ Unif.unifyWithTType err typ typeCand

checkExpected :: ErrorReporter -> ExpectedAtom -> TypeCand -> ExprEnv (CanFail TypeCand)
checkExpected err ttyp t = ExprEnv . lift $ Unif.checkExpected err ttyp t

typeCheckIn :: NodeEnvironment -> ExprEnv (CanFail (TEquation TypeCand)) -> NodeEnv (CanFail (TEquation TType))
typeCheckIn (NodeEnvironment {vMapping}) (ExprEnv expr) = NodeEnv . state $ \ns -> (runExpr ns, ns)
  where
    runExpr :: NodeState -> CanFail (TEquation TType)
    runExpr ns =
      let m = runReaderT expr (ns, vMapping)
          (tEq, f) = Unif.runTypeUnifier m
       in collapse $ traverseEq f <$> tEq

    traverseEq :: (Localized TypeCand -> CanFail TType) -> TEquation TypeCand -> CanFail (TEquation TType)
    traverseEq f (pat, e) = (pat,) <$> traverseExpr f e

    traverseExpr :: (Localized TypeCand -> CanFail TType) -> TExpr TypeCand -> CanFail (TExpr TType)
    traverseExpr f loc@(L _ (e, t) _) = (loc $>) <$> ((,) <$> traverseExprDesc f e <*> f (loc $> t))

    traverseExprDesc :: (Localized TypeCand -> CanFail TType) -> TExprDesc TypeCand -> CanFail (TExprDesc TType)
    traverseExprDesc _ (ConstantTExpr c) = pure $ ConstantTExpr c
    traverseExprDesc _ (VarTExpr v) = pure $ VarTExpr v
    traverseExprDesc f (UnOpTExpr op e) = UnOpTExpr op <$> traverseExpr f e
    traverseExprDesc f (BinOpTExpr op lhs rhs) = BinOpTExpr op <$> traverseExpr f lhs <*> traverseExpr f rhs
    traverseExprDesc f (AppTExpr nn args) = AppTExpr nn <$> traverse (traverseExpr f) args
    traverseExprDesc f (TupleTExpr l) = TupleTExpr <$> traverse (traverseExpr f) l
    traverseExprDesc f (IfTExpr {ifCond, ifTrue, ifFalse}) =
      IfTExpr <$> traverseExpr f ifCond <*> traverseExpr f ifTrue <*> traverseExpr f ifFalse
    traverseExprDesc f (FbyTExpr {fbyInit, fbyNext}) =
      FbyTExpr <$> traverseExpr f fbyInit <*> traverseExpr f fbyNext

-- # End Section
