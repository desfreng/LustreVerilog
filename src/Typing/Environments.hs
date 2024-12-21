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
    exprType,
    -- Unifying functions
    TypeUnifier (),
    TypeCand (),
    constantTypeCand,
    tupleTypeCand,
    fromAtomicType,
    nodeOutputTypeCand,
    unifyTypeCand,
    unifyAtomicCand,
    unifyWithTType,
    ExpectedAtom (),
    Unif.expectBitVector,
    Unif.expectBool,
    checkExpected,
  )
where

import Commons.Ast
import Commons.BiList
import Commons.Localized
import Commons.Tree
import Commons.Types
import Commons.TypingError
import Control.Applicative (Applicative (..))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Functor (($>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Data.String (IsString (..))
import Data.Text.Lazy (unpack)
import Typing.Ast
import Typing.AtomicUnifier (AtomicCand)
import Typing.Unification (ExpectedAtom, TypeCand, TypeUnifier)
import qualified Typing.Unification as Unif

-- # Section NodeEnv Monad

type NodeState = Map NodeIdent (CanFail NodeSignature)

type NodeWriter = CanFail [(NodeIdent, TNode)]

newtype NodeEnv a = NodeEnv (StateT NodeState (Writer NodeWriter) a)
  deriving (Functor, Applicative, Monad)

addNode :: Localized Ident -> NodeEnvironment -> CanFail [TNodeEq] -> NodeEnv ()
addNode nName (NodeEnvironment {nCtx}) eqs =
  let nodeId = NodeIdent nName
      tnode = TNode <$> nCtx <*> eqs
   in NodeEnv $ do
        s <- get
        case Map.lookup nodeId s of
          Just _ ->
            tell (reportError nName ("Node " <> show nName <> " is already declared."))
          Nothing ->
            put (Map.insert nodeId (buildSig <$> nCtx) s) >> tell (List.singleton . (,) nodeId <$> tnode)
  where
    buildSig n =
      let varTyp = tnodeVarTypes n
          inputTyp = (!) varTyp <$> tnodeInput n
          outputTyp =
            case (!) varTyp <$> tnodeOutput n of
              x :| [] -> TreeLeaf x
              x :| (y : l) -> TreeNode $ BiList (TreeLeaf x) (TreeLeaf y) (TreeLeaf <$> l)
       in NodeSignature (length inputTyp) inputTyp outputTyp

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv (NodeEnv m) = let (((), s), w) = runWriter $ runStateT m Map.empty in TAst <$> sequenceA s <*> w

-- # End Section

-- # Section NodeVarType Monad

type VarMapping = Map VarIdent (CanFail AtomicTType)

data VarKind = Input | Output | Local

data NodeVarState = NodeVarState {varList :: CanFail [(VarKind, VarIdent)], varMap :: VarMapping}

data NodeEnvironment = NodeEnvironment {nCtx :: CanFail TNodeContext, vMapping :: VarMapping}

newtype NodeVarEnv a = NodeVarEnv (State NodeVarState a)
  deriving (Functor, Applicative, Monad)

addVariable :: VarKind -> Localized Ident -> CanFail AtomicTType -> NodeVarEnv ()
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

    buildCtx (inL, outL, locL) = TNodeContext inL outL locL

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

findNode :: Localized Ident -> ExprEnv (CanFail (NodeIdent, NodeSignature))
findNode nodeName = ExprEnv $ asks (findNode' . fst)
  where
    nodeId = NodeIdent nodeName
    findNode' ns =
      case Map.lookup nodeId ns of
        Just sig -> (nodeId,) <$> sig
        Nothing ->
          let Ident s = unwrap nodeName
           in reportError nodeName $ "Unknown node " <> unpack s <> "."

findVariable :: Localized Ident -> ExprEnv (CanFail (VarIdent, AtomicTType))
findVariable varName = ExprEnv $ asks (findVariable' . snd)
  where
    varId = VarIdent varName
    findVariable' vs =
      case Map.lookup varId vs of
        Just sig -> (varId,) <$> sig
        Nothing ->
          let Ident s = unwrap varName
           in reportError varName $ "Unknown variable " <> unpack s <> "."

constantTypeCand :: Constant -> ExprEnv AtomicCand
constantTypeCand c = ExprEnv . lift $ Unif.constantTypeCand c

tupleTypeCand :: BiList TypeCand -> ExprEnv TypeCand
tupleTypeCand l = ExprEnv . lift $ Unif.tupleTypeCand l

fromAtomicType :: AtomicTType -> ExprEnv AtomicCand
fromAtomicType atom = ExprEnv . lift $ Unif.fromAtomicType atom

nodeOutputTypeCand :: NodeSignature -> ExprEnv TypeCand
nodeOutputTypeCand sig = ExprEnv . lift $ Unif.nodeOutputTypeCand sig

unifyTypeCand :: ErrorReporter -> TypeCand -> TypeCand -> ExprEnv (CanFail TypeCand)
unifyTypeCand err t1 t2 = ExprEnv . lift $ Unif.unify err t1 t2

unifyAtomicCand :: ErrorReporter -> AtomicCand -> AtomicCand -> ExprEnv (CanFail AtomicCand)
unifyAtomicCand err t1 t2 = ExprEnv . lift $ Unif.unifyAtom err t1 t2

unifyWithTType :: ErrorReporter -> TType -> TypeCand -> ExprEnv (CanFail TypeCand)
unifyWithTType err typ typeCand = ExprEnv . lift $ Unif.unifyWithTType err typ typeCand

checkExpected :: ErrorReporter -> ExpectedAtom -> TypeCand -> ExprEnv (CanFail AtomicCand)
checkExpected err ttyp t = ExprEnv . lift $ Unif.checkExpected err ttyp t

asTC :: AtomicCand -> ExprEnv TypeCand
asTC = ExprEnv . lift . Unif.asTypeCand

exprType :: TExprKind TypeCand AtomicCand -> ExprEnv TypeCand
exprType (ConstantTExpr _ aT) = asTC aT
exprType (VarTExpr _ aT) = asTC aT
exprType (UnOpTExpr _ _ aT) = asTC aT
exprType (BinOpTExpr _ _ _ aT) = asTC aT
exprType (AppTExpr _ _ t) = return t
exprType (TupleTExpr _ t) = return t
exprType (IfTExpr {ifTyp}) = return ifTyp
exprType (FbyTExpr {fbyTyp}) = return fbyTyp

typeCheckIn :: NodeEnvironment -> ExprEnv (CanFail (TEquation TypeCand AtomicCand)) -> NodeEnv (CanFail TNodeEq)
typeCheckIn (NodeEnvironment {vMapping}) m = NodeEnv . gets $ runExpr vMapping m

runExpr :: VarMapping -> ExprEnv (CanFail (TEquation TypeCand AtomicCand)) -> NodeState -> CanFail TNodeEq
runExpr vMap (ExprEnv expr) ns = collapse $ sEq <$> tEq
  where
    (tEq, sTyp, sAtom) = Unif.runTypeUnifier $ runReaderT expr (ns, vMap)

    sEq (pat, e) = (pat,) <$> sExpr e
    sExpr e = (e $>) <$> sExprDesc (reportError e) (unwrap e)

    sExprDesc :: ErrorReporter -> TExprKind TypeCand AtomicCand -> CanFail (TExprKind TType AtomicTType)
    sExprDesc err (ConstantTExpr c aT) =
      ConstantTExpr c <$> Unif.sanitizeAtom err sAtom aT
    sExprDesc err (VarTExpr vId aT) =
      VarTExpr vId <$> Unif.sanitizeAtom err sAtom aT
    sExprDesc err (UnOpTExpr op e aT) =
      UnOpTExpr op <$> sExpr e <*> Unif.sanitizeAtom err sAtom aT
    sExprDesc err (BinOpTExpr op lhs rhs aT) =
      BinOpTExpr op <$> sExpr lhs <*> sExpr rhs <*> Unif.sanitizeAtom err sAtom aT
    sExprDesc err (AppTExpr nn args t) =
      AppTExpr nn <$> traverse sExpr args <*> Unif.sanitize err sTyp sAtom t
    sExprDesc err (TupleTExpr l t) =
      TupleTExpr <$> traverse sExpr l <*> Unif.sanitize err sTyp sAtom t
    sExprDesc err (IfTExpr {ifCond, ifTrue, ifFalse, ifTyp}) =
      IfTExpr <$> sExpr ifCond <*> sExpr ifTrue <*> sExpr ifFalse <*> Unif.sanitize err sTyp sAtom ifTyp
    sExprDesc err (FbyTExpr {fbyInit, fbyNext, fbyTyp}) =
      FbyTExpr <$> sExpr fbyInit <*> sExpr fbyNext <*> Unif.sanitize err sTyp sAtom fbyTyp

-- # End Section
