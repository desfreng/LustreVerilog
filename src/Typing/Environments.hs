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
    TypeCand (),
    AtomicCand (),
    constantTypeCand,
    tupleTypeCand,
    fromAtomicType,
    nodeOutputTypeCand,
    unifyTypeCand,
    unifyAtomicCand,
    unifyWithTType,
    ExpectedAtom (),
    U.expectBitVector,
    U.expectBool,
    checkExpected,
  )
where

import Commons.Ast
import Commons.BiList
import Commons.Ids
import Commons.Position
import Commons.Tree
import Commons.Types
import Commons.TypingError
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
import Typing.TypeUnification (AtomicCand, AtomicUnifier, ExpectedAtom, TypeCand)
import qualified Typing.TypeUnification as U

-- # Section NodeEnv Monad

type NodeState = Map NodeIdent (CanFail NodeSignature)

type NodeWriter = CanFail [(NodeIdent, TNode)]

newtype NodeEnv a = NodeEnv (StateT NodeState (Writer NodeWriter) a)
  deriving (Functor, Applicative, Monad)

addNode :: Pos Ident -> NodeEnvironment -> CanFail (NonEmpty TNodeEq) -> NodeEnv ()
addNode nName (NodeEnvironment {nCtx}) eqs =
  let nodeId = NodeIdent nName
      tnode = Node <$> nCtx <*> eqs
   in NodeEnv $ do
        s <- get
        case Map.lookup nodeId s of
          Just _ ->
            tell (reportError nName ("Node " <> show nName <> " is already declared."))
          Nothing ->
            put (Map.insert nodeId (buildSig <$> nCtx) s) >> tell (List.singleton . (,) nodeId <$> tnode)
  where
    buildSig n =
      let varTyp = nodeVarTypes n
          inputTyp = (!) varTyp <$> nodeInput n
          outputTyp =
            case (!) varTyp <$> nodeOutput n of
              x :| [] -> TreeLeaf x
              x :| (y : l) -> TreeNode $ BiList (TreeLeaf x) (TreeLeaf y) (TreeLeaf <$> l)
       in NodeSignature (length inputTyp) inputTyp outputTyp

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv (NodeEnv m) = let (((), s), w) = runWriter $ runStateT m Map.empty in Ast <$> sequenceA s <*> w

-- # End Section

-- # Section NodeVarType Monad

type VarMapping = Map VarIdent (CanFail AtomicTType)

data VarKind = Input | Output | Local

data NodeVarState = NodeVarState {varList :: CanFail [(VarKind, VarIdent)], varMap :: VarMapping}

data NodeEnvironment = NodeEnvironment {nCtx :: CanFail (NodeContext VarIdent), vMapping :: VarMapping}

newtype NodeVarEnv a = NodeVarEnv (State NodeVarState a)
  deriving (Functor, Applicative, Monad)

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

newtype ExprEnv a = ExprEnv (ReaderT (NodeState, VarMapping) AtomicUnifier a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (ExprEnv a) where
  (<>) :: ExprEnv a -> ExprEnv a -> ExprEnv a
  (<>) = liftA2 (<>)

instance IsString (ExprEnv String) where
  fromString :: String -> ExprEnv String
  fromString = pure

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

constantTypeCand :: Constant -> ExprEnv AtomicCand
constantTypeCand c = ExprEnv . lift $ U.constantTypeCand c

tupleTypeCand :: BiList TypeCand -> ExprEnv TypeCand
tupleTypeCand l = ExprEnv . lift $ U.tupleTypeCand l

fromAtomicType :: AtomicTType -> ExprEnv AtomicCand
fromAtomicType atom = ExprEnv . lift $ U.fromAtomicType atom

nodeOutputTypeCand :: NodeSignature -> ExprEnv TypeCand
nodeOutputTypeCand sig = ExprEnv . lift $ U.nodeOutputTypeCand sig

unifyTypeCand :: ErrorReporter -> TypeCand -> TypeCand -> ExprEnv (CanFail TypeCand)
unifyTypeCand err t1 t2 = ExprEnv . lift $ U.unifyTypeCand err t1 t2

unifyAtomicCand :: ErrorReporter -> AtomicCand -> AtomicCand -> ExprEnv (CanFail AtomicCand)
unifyAtomicCand err t1 t2 = ExprEnv . lift $ U.unify err t1 t2

unifyWithTType :: ErrorReporter -> TType -> TypeCand -> ExprEnv (CanFail TypeCand)
unifyWithTType err typ typeCand = ExprEnv . lift $ U.unifyWithTType err typ typeCand

checkExpected :: ErrorReporter -> ExpectedAtom -> TypeCand -> ExprEnv (CanFail AtomicCand)
checkExpected err ttyp t = ExprEnv . lift $ U.checkExpected err ttyp t

asTC :: AtomicCand -> ExprEnv TypeCand
asTC = ExprEnv . lift . U.asTC

exprType :: TExprKind TypeCand AtomicCand -> ExprEnv TypeCand
exprType (ConstantTExpr _ aT) = asTC aT
exprType (VarTExpr _ aT) = asTC aT
exprType (UnOpTExpr _ _ aT) = asTC aT
exprType (BinOpTExpr _ _ _ aT) = asTC aT
exprType (IfTExpr _ _ _ t) = return t
exprType (AppTExpr _ _ t) = return t
exprType (TupleTExpr _ t) = return t
exprType (FbyTExpr {fbyTyp}) = return fbyTyp

typeCheckIn :: NodeEnvironment -> ExprEnv (CanFail (TEquation TypeCand AtomicCand)) -> NodeEnv (CanFail TNodeEq)
typeCheckIn (NodeEnvironment {vMapping}) m = NodeEnv . gets $ runExpr vMapping m

runExpr :: VarMapping -> ExprEnv (CanFail (TEquation TypeCand AtomicCand)) -> NodeState -> CanFail TNodeEq
runExpr vMap (ExprEnv expr) ns = collapse $ sEq <$> tEq
  where
    (tEq, sAtom) = U.runAtomicUnifier $ runReaderT expr (ns, vMap)

    sEq (pat, e) = (pat,) <$> sExpr e
    sExpr e = (e $>) <$> sExprDesc (reportError e) (unwrap e)

    sExprDesc :: ErrorReporter -> TExprKind TypeCand AtomicCand -> CanFail (TExprKind TType AtomicTType)
    sExprDesc err (ConstantTExpr c aT) =
      ConstantTExpr c <$> U.sanitizeAtom err sAtom aT
    sExprDesc err (VarTExpr vId aT) =
      VarTExpr vId <$> U.sanitizeAtom err sAtom aT
    sExprDesc err (UnOpTExpr op e aT) =
      UnOpTExpr op <$> sExpr e <*> U.sanitizeAtom err sAtom aT
    sExprDesc err (BinOpTExpr op lhs rhs aT) =
      BinOpTExpr op <$> sExpr lhs <*> sExpr rhs <*> U.sanitizeAtom err sAtom aT
    sExprDesc err (IfTExpr ifCond ifTrue ifFalse ifTyp) =
      IfTExpr <$> sExpr ifCond <*> sExpr ifTrue <*> sExpr ifFalse <*> U.sanitize err sAtom ifTyp
    sExprDesc err (AppTExpr nn args t) =
      AppTExpr nn <$> traverse sExpr args <*> U.sanitize err sAtom t
    sExprDesc err (TupleTExpr l t) =
      TupleTExpr <$> traverse sExpr l <*> U.sanitize err sAtom t
    sExprDesc err (FbyTExpr {fbyInit, fbyNext, fbyTyp}) =
      FbyTExpr <$> sExpr fbyInit <*> sExpr fbyNext <*> U.sanitize err sAtom fbyTyp

-- # End Section
