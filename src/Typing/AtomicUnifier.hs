{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Typing.AtomicUnifier
  ( -- TypeUnifier Monad
    AtomicUnifier (),
    runAtomicUnifier,
    unifyWithTType,
    -- Sanitization
    AtomicState (),
    sanitizeAtom,
    -- AtomicCand
    AtomicCand (),
    constantTypeCand,
    fromAtomicType,
    -- ExpectedAtom
    ExpectedAtom (),
    expectBitVector,
    expectBool,
    checkExpected,
  )
where

import Commons.Ast (BVSize (..), BitVectorKind (..), Constant (..))
import Commons.Types (AtomicTType (..), ppAtomicType)
import Commons.TypingError (CanFail, ErrorReporter, embed)
import Control.Applicative (Applicative (..))
import Control.Monad (zipWithM)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (MonadState (state), State, StateT (StateT), runState)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (find)
import Data.Functor (($>))
import Data.List (intercalate, intersect)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import GHC.Num (integerLog2)
import Typing.MonadUnif (MonadAssocReader (..), MonadUnif (..), UnifState, emptyState)

newtype UnsignedSize = UnSig BVSize
  deriving (Show, Eq, Ord)

newtype SignedSize = Sig BVSize
  deriving (Show, Eq, Ord)

data AtomKind
  = BitVector BitVectorKind BVSize
  | Numeric (NonEmpty BitVectorKind) UnsignedSize SignedSize
  | Bool
  | Custom () [AtomicCand]
  deriving (Show)

newtype AtomicCand = AtomicCand Int
  deriving (Show, Eq, Ord, Enum)

data ExpectedKind = BoolExp | UnsizedBitVector BitVectorKind
  deriving (Eq)

instance Show ExpectedKind where
  show :: ExpectedKind -> String
  show BoolExp = "bool"
  show (UnsizedBitVector Raw) = "r??"
  show (UnsizedBitVector Unsigned) = "u??"
  show (UnsizedBitVector Signed) = "i??"

newtype ExpectedAtom = ExpectedAtom (NonEmpty ExpectedKind)
  deriving (Semigroup)

instance Show ExpectedAtom where
  show :: ExpectedAtom -> String
  show (ExpectedAtom atom) = intercalate "|" (NonEmpty.toList $ show <$> atom)

type AtomicState = UnifState AtomicCand AtomKind

data AUState = AUState {unifState :: AtomicState, nextId :: AtomicCand}

newtype AtomicUnifier a = AtomicUnifier (State AUState a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (AtomicUnifier a) where
  (<>) :: AtomicUnifier a -> AtomicUnifier a -> AtomicUnifier a
  (<>) = liftA2 (<>)

instance (IsString a) => IsString (AtomicUnifier a) where
  fromString :: (IsString a) => String -> AtomicUnifier a
  fromString = pure . fromString

instance MonadState AtomicState AtomicUnifier where
  state :: (UnifState AtomicCand AtomKind -> (a, UnifState AtomicCand AtomKind)) -> AtomicUnifier a
  state f = AtomicUnifier . state $ \s -> let (x, s') = f (unifState s) in (x, s {unifState = s'})

instance MonadUnif AtomicCand AtomKind AtomicUnifier where
  {-# INLINEABLE subVars #-}
  subVars :: AtomKind -> [AtomicCand]
  subVars Bool = []
  subVars BitVector {} = []
  subVars Numeric {} = []
  subVars (Custom _ l) = l

  {-# INLINEABLE idName #-}
  idName :: AtomicUnifier String
  idName = "atomic type"

  showId :: AtomicCand -> AtomicUnifier String
  showId t = unfoldVal t >>= go
    where
      go Nothing = return "?"
      go (Just x) = showAtom' x

      showAtom' Bool = return "bool"
      showAtom' (BitVector Raw (BVSize i)) = return $ "r" <> show i
      showAtom' (BitVector Unsigned (BVSize i)) = return $ "u" <> show i
      showAtom' (BitVector Signed (BVSize i)) = return $ "i" <> show i
      showAtom' (Numeric numKind uS sS) =
        return $ intercalate "|" . NonEmpty.toList $ showNumKind uS sS <$> numKind
      showAtom' (Custom x l) = do
        lStr <- mapM showId l
        let lStrParens s = "(" <> s <> ")"
            customStr = show x
         in return $ "Custom " <> customStr <> " " <> unwords (lStrParens <$> lStr)

      showNumKind (UnSig (BVSize uS)) _ Raw = "r(>=" <> show uS <> ")"
      showNumKind (UnSig (BVSize uS)) _ Unsigned = "u(>=" <> show uS <> ")"
      showNumKind _ (Sig (BVSize sS)) Signed = "i(>=" <> show sS <> ")"

  unifyCand ::
    ErrorReporter -> (AtomicCand, AtomKind) -> (AtomicCand, AtomKind) -> AtomicUnifier (CanFail AtomKind)
  unifyCand err (r1, t1) (r2, t2) =
    case (t1, t2) of
      -- Bool
      (Bool, Bool) -> return $ pure Bool
      (Bool, BitVector {}) -> reportErr
      (Bool, Numeric {}) -> reportErr
      (Bool, Custom {}) -> reportErr
      -- BitVector
      (BitVector {}, Bool) -> reportErr
      (BitVector kind1 size1, BitVector kind2 size2) -> unifyBV kind1 kind2 size1 size2
      (BitVector kind size, Numeric numKind minUSize minSSize) -> unifyBVNum kind size numKind minUSize minSSize
      (BitVector {}, Custom {}) -> reportErr
      -- Numeric
      (Numeric {}, Bool) -> reportErr
      (Numeric k1 uS1 sS1, Numeric k2 uS2 sS2) -> unifyNumNum k1 uS1 sS1 k2 uS2 sS2
      (Numeric numKind minUSize minSSize, BitVector kind size) -> unifyBVNum kind size numKind minUSize minSSize
      (Numeric {}, Custom {}) -> reportErr
      -- Custom
      (Custom {}, Bool) -> reportErr
      (Custom {}, BitVector {}) -> reportErr
      (Custom {}, Numeric {}) -> reportErr
      (Custom tId1 args1, Custom tId2 args2) -> unifyCustom tId1 tId2 args1 args2
    where
      reportErr = err <$> "Cannot unify atomic type " <> showId r1 <> " with " <> showId r2 <> "."

      unifyBV kind1 kind2 size1 size2 =
        if kind1 == kind2 && size1 == size2
          then return $ pure t1
          else reportErr

      unifyBVNum kind size numKind minUSize minSSize =
        if kind `elem` numKind && isSizeCompatible size kind minUSize minSSize
          then return $ pure (BitVector kind size)
          else reportErr

      unifyNumNum k1 uS1 sS1 k2 uS2 sS2 =
        case filterKind k1 (NonEmpty.toList k2) of
          Just k -> return . pure $ Numeric k (max uS1 uS2) (max sS1 sS2)
          Nothing -> reportErr

      unifyCustom tId1 tId2 args1 args2 =
        if tId1 == tId2
          then fmap (Custom tId1) . sequenceA <$> zipWithM (unify err) args1 args2
          else reportErr

filterKind :: NonEmpty BitVectorKind -> [BitVectorKind] -> Maybe (NonEmpty BitVectorKind)
filterKind l1 l2 = NonEmpty.nonEmpty (NonEmpty.toList l1 `intersect` l2)

isSizeCompatible :: BVSize -> BitVectorKind -> UnsignedSize -> SignedSize -> Bool
isSizeCompatible size Raw (UnSig minSize) _ = size >= minSize
isSizeCompatible size Unsigned (UnSig minSize) _ = size >= minSize
isSizeCompatible size Signed _ (Sig minSize) = size >= minSize

unifyWithTType :: ErrorReporter -> AtomicTType -> AtomicCand -> AtomicUnifier (CanFail AtomicCand)
unifyWithTType err typ t = do
  v <- unfoldVal t
  case v of
    Nothing -> fromAtomicType typ >>= (t ~>)
    Just k -> unifyWithTType' t typ k >>= embed . fmap (setVal t)
  where
    reportErr r = err <$> "Cannot unify atomic type " <> showId r <> " with " <> pure (ppAtomicType typ) <> "."

    unifyWithTType' r ty k =
      case (ty, k) of
        -- TBool
        (TBool, Bool) -> return $ pure Bool
        (TBool, BitVector {}) -> reportErr r
        (TBool, Numeric {}) -> reportErr r
        (TBool, Custom {}) -> reportErr r
        -- TBitVector
        (TBitVector {}, Bool) -> reportErr r
        (TBitVector typKind typSize, BitVector candKind candSize) -> unifyBV r typKind typSize candKind candSize
        (TBitVector typKind typSize, Numeric numKind uS sS) -> unifyBVNum r typKind typSize numKind uS sS
        (TBitVector {}, Custom {}) -> reportErr r
        -- TCustom
        (TCustom {}, BitVector {}) -> reportErr r
        (TCustom {}, Numeric {}) -> reportErr r
        (TCustom {}, Bool) -> reportErr r
        (TCustom tId1 args1, Custom tId2 args2) -> unifyCustom r tId1 args1 tId2 args2

    unifyBV r typKind typSize candKind candSize =
      if typKind == candKind && typSize == candSize
        then return $ pure (BitVector typKind typSize)
        else reportErr r

    unifyBVNum r typKind typSize numKind uS sS =
      if typKind `elem` numKind && isSizeCompatible typSize typKind uS sS
        then return $ pure (BitVector typKind typSize)
        else reportErr r

    unifyCustom r tId1 args1 tId2 args2 =
      if tId1 == tId2
        then fmap (Custom tId1) . sequenceA <$> zipWithM (unifyWithTType err) args1 args2
        else reportErr r

checkExpected :: ErrorReporter -> ExpectedAtom -> AtomicCand -> AtomicUnifier (CanFail AtomicCand)
checkExpected err expAtom@(ExpectedAtom l) t = do
  v <- unfoldVal t
  case v of
    Nothing -> err <$> "Type variable " <> showId t <> " is not " <> pure (show expAtom) <> "."
    Just k ->
      case checkAtom k of
        Nothing -> err <$> "Unable to unify " <> showId t <> " with " <> pure (show expAtom) <> "."
        Just k' -> pure <$> setVal t k'
  where
    checkAtom v@Bool = find (== BoolExp) l $> v
    checkAtom v@(BitVector kind size) = find (isCompatibleBitVector kind size) l $> v
    checkAtom (Numeric nK uS sS) =
      let buildNumeric u s k = Numeric k u s
       in buildNumeric uS sS <$> filterKind nK (mapMaybe getBitVectorKind (NonEmpty.toList l))
    checkAtom (Custom {}) = Nothing

    isCompatibleBitVector _ _ BoolExp = False
    isCompatibleBitVector kind _ (UnsizedBitVector expKind) = kind == expKind

    getBitVectorKind BoolExp = Nothing
    getBitVectorKind (UnsizedBitVector k) = Just k

buildAndBind :: AtomKind -> AtomicUnifier AtomicCand
buildAndBind v = do
  newVar <- AtomicUnifier $ state (\s -> (nextId s, s {nextId = incr (nextId s)}))
  setVal newVar v
  where
    incr (AtomicCand x) = AtomicCand (x + 1)

constantTypeCand :: Constant -> AtomicUnifier AtomicCand
constantTypeCand = buildAndBind . go
  where
    go (BoolConst _) = Bool
    go (IntegerConst i) =
      let log2 = fromEnum (integerLog2 i)
          minUSize = UnSig (BVSize (1 + log2))
          minSSize = Sig (BVSize (2 + log2))
       in Numeric (Raw :| [Unsigned, Signed]) minUSize minSSize

fromAtomicType :: AtomicTType -> AtomicUnifier AtomicCand
fromAtomicType TBool = buildAndBind Bool
fromAtomicType (TBitVector kind size) = buildAndBind (BitVector kind size)
fromAtomicType (TCustom tId args) = mapM fromAtomicType args >>= buildAndBind . Custom tId

expectBitVector :: BitVectorKind -> ExpectedAtom
expectBitVector = ExpectedAtom . NonEmpty.singleton . UnsizedBitVector

expectBool :: ExpectedAtom
expectBool = ExpectedAtom (NonEmpty.singleton BoolExp)

runAtomicUnifier :: AtomicUnifier a -> (a, AtomicState)
runAtomicUnifier (AtomicUnifier m) = second unifState $ runState m initialState
  where
    initialState = AUState {unifState = emptyState, nextId = AtomicCand 0}

instance MonadAssocReader AtomicCand AtomKind (Reader AtomicState)

sanitizeAtom :: ErrorReporter -> AtomicState -> AtomicCand -> CanFail AtomicTType
sanitizeAtom err st x = runReader (go x) st
  where
    go :: AtomicCand -> Reader AtomicState (CanFail AtomicTType)
    go tCand = val tCand >>= sanitizeAtomicCand

    sanitizeAtomicCand Nothing = return $ err "No final type found."
    sanitizeAtomicCand (Just t) = sanitizeAtom' t

    sanitizeAtom' Bool = return $ pure TBool
    sanitizeAtom' (BitVector kind size) = return $ pure (TBitVector kind size)
    sanitizeAtom' (Numeric {}) = return $ err "Unable to find the bit-vector size of this expression."
    sanitizeAtom' (Custom tId args) = fmap (TCustom tId) . sequenceA <$> mapM go args
