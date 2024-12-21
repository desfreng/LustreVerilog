{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Typing.Unification
  ( -- TypeUnifier Monad
    TypeUnifier (),
    runTypeUnifier,
    -- Sanitization
    AtomicState (),
    TypeState (),
    AU.sanitizeAtom,
    sanitize,
    -- TypeCandidate type
    TypeCand (),
    constantTypeCand,
    tupleTypeCand,
    fromAtomicType,
    asTypeCand,
    nodeOutputTypeCand,
    unify,
    unifyAtom,
    unifyWithTType,
    -- ExpectedAtom
    ExpectedAtom (),
    AU.expectBitVector,
    AU.expectBool,
    checkExpected,
  )
where

import Commons.Ast (Constant)
import Commons.BiList (BiList)
import qualified Commons.BiList as BiList
import Commons.Tree (Tree (TreeLeaf, TreeNode))
import Commons.Types (AtomicTType, NodeSignature (outputType), TType, ppTType)
import Commons.TypingError (CanFail, ErrorReporter, embed)
import Control.Applicative (Applicative (..))
import Control.Monad.Reader (MonadTrans (lift), Reader, runReader)
import Control.Monad.State (MonadState (state), StateT (..))
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import Data.String (IsString (..))
import Typing.AtomicUnifier (AtomicCand, AtomicState, AtomicUnifier, ExpectedAtom)
import qualified Typing.AtomicUnifier as AU
import Typing.MonadUnif (MonadAssocReader (..), MonadUnif (..), UnifState, emptyState)
import qualified Typing.MonadUnif as AU

data TypeKind
  = Tuple (BiList TypeCand)
  | Atomic AtomicCand
  deriving (Show)

newtype TypeCand = TypeCand Int
  deriving (Show, Eq, Ord, Enum)

type TypeState = UnifState TypeCand TypeKind

data TUState = AUState {unifState :: TypeState, nextId :: TypeCand}

newtype TypeUnifier a = TypeUnifier (StateT TUState AtomicUnifier a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (TypeUnifier a) where
  (<>) :: TypeUnifier a -> TypeUnifier a -> TypeUnifier a
  (<>) = liftA2 (<>)

instance IsString (TypeUnifier String) where
  fromString :: String -> TypeUnifier String
  fromString = pure

instance MonadState TypeState TypeUnifier where
  state :: (TypeState -> (a, TypeState)) -> TypeUnifier a
  state f = TypeUnifier . state $ \s -> let (x, s') = f (unifState s) in (x, s {unifState = s'})

tupleErr :: ErrorReporter -> Int -> Int -> TypeUnifier String -> TypeUnifier String -> TypeUnifier (CanFail a)
tupleErr err ll1 ll2 t1 t2 =
  err
    <$> "Cannot unify tuples of different lengths: "
      <> pure (show ll1)
      <> " and "
      <> pure (show ll2)
      <> ".\nTypes are: "
      <> t1
      <> " and "
      <> t2
      <> "."

instance MonadUnif TypeCand TypeKind TypeUnifier where
  subVars :: TypeKind -> [TypeCand]
  subVars (Atomic _) = []
  subVars (Tuple l) = toList l

  idName :: TypeUnifier String
  idName = "type"

  showId :: TypeCand -> TypeUnifier String
  showId t = unfoldVal t >>= go
    where
      go :: Maybe TypeKind -> TypeUnifier String
      go Nothing = return "?"
      go (Just (Atomic x)) = TypeUnifier . lift $ showId x
      go (Just (Tuple l)) = do
        lStr <- toList <$> mapM showId l
        return $ "(" <> intercalate ", " lStr <> ")"

  unifyCand :: ErrorReporter -> (TypeCand, TypeKind) -> (TypeCand, TypeKind) -> TypeUnifier (CanFail TypeKind)
  unifyCand err (r1, t1) (r2, t2) =
    case (t1, t2) of
      (Tuple l1, Tuple l2) ->
        let ll1 = length l1
            ll2 = length l2
         in if ll1 == ll2
              then fmap Tuple . sequenceA <$> BiList.zipWithM (unify err) l1 l2
              else tupleErr err ll1 ll2 (showId r1) (showId r2)
      (Atomic a1, Atomic a2) -> fmap Atomic <$> TypeUnifier (lift (unify err a1 a2))
      (Tuple _, Atomic _) -> reportErr
      (Atomic _, Tuple _) -> reportErr
    where
      reportErr = err <$> "Cannot unify the type " <> showId r1 <> " with the type " <> showId r2 <> "."

unifyWithTType :: ErrorReporter -> TType -> TypeCand -> TypeUnifier (CanFail TypeCand)
unifyWithTType err typ r = do
  t <- unfoldVal r
  case t of
    Nothing -> candFromTType typ >>= (r ~>)
    Just t' -> unifyWithTType' typ t' >>= embed . fmap (setVal r)
  where
    reportErr = err <$> "Unable to unify " <> showId r <> " with " <> pure (ppTType typ) <> "."

    unifyWithTType' ttyp t =
      case (ttyp, t) of
        (TreeNode tL, Tuple cL) -> unifyNode tL cL
        (TreeLeaf tA, Atomic cA) -> unifyLeaf tA cA
        (TreeNode _, Atomic _) -> reportErr
        (TreeLeaf _, Tuple _) -> reportErr

    unifyLeaf typAtom candAtom =
      fmap Atomic <$> TypeUnifier (lift (AU.unifyWithTType err typAtom candAtom))

    unifyNode typL candL =
      let typLength = length typL
          candLength = length candL
       in if candLength == typLength
            then fmap Tuple . sequenceA <$> BiList.zipWithM (unifyWithTType err) typL candL
            else tupleErr err typLength candLength (showId r) (pure (ppTType typ))

candFromTType :: TType -> TypeUnifier TypeCand
candFromTType (TreeLeaf x) = TypeUnifier (lift (AU.fromAtomicType x)) >>= buildAndBind . Atomic
candFromTType (TreeNode l) = mapM candFromTType l >>= buildAndBind . Tuple

checkExpected :: ErrorReporter -> ExpectedAtom -> TypeCand -> TypeUnifier (CanFail AtomicCand)
checkExpected err expAtom x = do
  t <- unfoldVal x
  case t of
    Nothing -> err <$> "Type variable " <> showId x <> " is not " <> pure (show expAtom) <> "."
    Just t' -> checkExpected' t' expAtom
  where
    checkExpected' (Tuple _) _ = err <$> "Unable to unify " <> showId x <> " with " <> pure (show expAtom) <> "."
    checkExpected' (Atomic atom) expA =
      TypeUnifier (lift (AU.checkExpected err expA atom)) >>= embed . fmap (\c -> setVal x (Atomic c) >> return c)

buildAndBind :: TypeKind -> TypeUnifier TypeCand
buildAndBind v = do
  newVar <- TypeUnifier $ state (\s -> (nextId s, s {nextId = incr (nextId s)}))
  setVal newVar v
  where
    incr (TypeCand x) = TypeCand (x + 1)

constantTypeCand :: Constant -> TypeUnifier AtomicCand
constantTypeCand c = TypeUnifier (lift (AU.constantTypeCand c))

unifyAtom :: ErrorReporter -> AtomicCand -> AtomicCand -> TypeUnifier (CanFail AtomicCand)
unifyAtom err aT1 aT2 = TypeUnifier (lift (AU.unify err aT1 aT2))

fromAtomicType :: AtomicTType -> TypeUnifier AtomicCand
fromAtomicType t = TypeUnifier (lift (AU.fromAtomicType t))

nodeOutputTypeCand :: NodeSignature -> TypeUnifier TypeCand
nodeOutputTypeCand = candFromTType . outputType

tupleTypeCand :: BiList TypeCand -> TypeUnifier TypeCand
tupleTypeCand = buildAndBind . Tuple

asTypeCand :: AtomicCand -> TypeUnifier TypeCand
asTypeCand = buildAndBind . Atomic

runTypeUnifier :: TypeUnifier a -> (a, TypeState, AtomicState)
runTypeUnifier (TypeUnifier m) =
  let ((x, s), sanitizeAtom) = AU.runAtomicUnifier (runStateT m initialState)
   in (x, unifState s, sanitizeAtom)
  where
    initialState = AUState {unifState = emptyState, nextId = TypeCand 0}

instance MonadAssocReader TypeCand TypeKind (Reader TypeState)

sanitize :: ErrorReporter -> TypeState -> AtomicState -> TypeCand -> CanFail TType
sanitize err sTyp sAtom t = runReader (go t) sTyp
  where
    go :: TypeCand -> Reader TypeState (CanFail TType)
    go tCand = val tCand >>= sanitizeTypeCand

    sanitizeTypeCand Nothing = return $ err "No final type found."
    sanitizeTypeCand (Just (Tuple l)) = fmap TreeNode . sequenceA <$> mapM go l
    sanitizeTypeCand (Just (Atomic a)) = return $ TreeLeaf <$> AU.sanitizeAtom err sAtom a
