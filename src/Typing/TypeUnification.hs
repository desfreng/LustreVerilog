{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Typing.TypeUnification
  ( -- TypeUnifier Monad
    TypeUnifier (),
    runUnifier,
    unifyTypeCand,
    showTypeCand,
    -- TypeCand
    TypeCand (),
    constantTypeCand,
    fromAtomicType,
    -- ExpectedType
    ExpectedType (),
    checkExpected,
    -- ExpectedList
    ExpectedList (),
    toAtom,
    expectBitVector,
    expectBool,
    -- ExpectedType
    fromType,
  )
where

import Commons.Ast (Constant (..))
import Commons.Ids (TypeIdent)
import Commons.Position
import Commons.Types (AtomicTType (..), BVSize (..), BitVectorKind (..))
import Commons.TypingError (CanFail, embed, reportError)
import qualified Control.Monad as Monad
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (MonadState (state), State, StateT (StateT), runState)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (find)
import Data.Functor (($>), (<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate, intersect)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.String (IsString (..))
import GHC.Num (integerLog2)
import Typing.MonadUnif (ConvertibleToInt (..), MonadAssocReader (..), MonadUnif (..), UnifState, emptyState)

newtype UnsignedSize = UnSig BVSize
  deriving (Show, Eq, Ord)

newtype SignedSize = Sig BVSize
  deriving (Show, Eq, Ord)

data TypeKind
  = BitVector BitVectorKind BVSize
  | Numeric (NonEmpty BitVectorKind) UnsignedSize SignedSize
  | Bool
  | Custom TypeIdent [TypeCand]

newtype TypeCand = TypeCand Int
  deriving (Eq, Ord, Enum)

instance ConvertibleToInt TypeCand where
  toInt :: TypeCand -> Int
  toInt (TypeCand i) = i

data ExpectedKind = BoolExp | UnsizedBitVector BitVectorKind
  deriving (Eq)

instance Show ExpectedKind where
  show :: ExpectedKind -> String
  show BoolExp = "bool"
  show (UnsizedBitVector Raw) = "r??"
  show (UnsizedBitVector Unsigned) = "u??"
  show (UnsizedBitVector Signed) = "i??"

newtype ExpectedList = ExpectedList (NonEmpty ExpectedKind)
  deriving (Semigroup)

data ExpectedType = EList ExpectedList | EType AtomicTType

instance Show ExpectedList where
  show :: ExpectedList -> String
  show (ExpectedList atom) = intercalate "|" (NonEmpty.toList $ show <$> atom)

instance Show ExpectedType where
  show :: ExpectedType -> String
  show (EList l) = show l
  show (EType typ) = show typ

type StateUnifier = UnifState TypeCand TypeKind

data UState = UState {unifState :: StateUnifier, nextId :: TypeCand, typCandPos :: IntMap (Pos ())}

newtype TypeUnifier a = AtomicUnifier (State UState a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (TypeUnifier a) where
  (<>) :: TypeUnifier a -> TypeUnifier a -> TypeUnifier a
  (<>) = liftA2 (<>)

instance (IsString a) => IsString (TypeUnifier a) where
  fromString :: (IsString a) => String -> TypeUnifier a
  fromString = pure . fromString

instance MonadState StateUnifier TypeUnifier where
  state :: (UnifState TypeCand TypeKind -> (a, UnifState TypeCand TypeKind)) -> TypeUnifier a
  state f = AtomicUnifier . state $ \s -> let (x, s') = f (unifState s) in (x, s {unifState = s'})

instance MonadUnif TypeCand TypeKind TypeUnifier where
  {-# INLINEABLE subVars #-}
  subVars :: TypeKind -> [TypeCand]
  subVars Bool = []
  subVars BitVector {} = []
  subVars Numeric {} = []
  subVars (Custom _ l) = l

  {-# INLINEABLE idName #-}
  idName :: TypeUnifier String
  idName = "atomic type"

  showId :: TypeCand -> TypeUnifier String
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
    Pos a -> (TypeCand, TypeKind) -> (TypeCand, TypeKind) -> TypeUnifier (CanFail TypeKind)
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
      reportErr = reportError err <$> "Cannot unify atomic type " <> showId r1 <> " with " <> showId r2 <> "."

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
          then fmap (Custom tId1) . sequenceA <$> Monad.zipWithM (unify err) args1 args2
          else reportErr

unifyTypeCand :: Pos a -> TypeCand -> TypeCand -> TypeUnifier (CanFail TypeCand)
unifyTypeCand = unify

showTypeCand :: TypeCand -> TypeUnifier String
showTypeCand = showId

filterKind :: NonEmpty BitVectorKind -> [BitVectorKind] -> Maybe (NonEmpty BitVectorKind)
filterKind l1 l2 = NonEmpty.nonEmpty (NonEmpty.toList l1 `intersect` l2)

isSizeCompatible :: BVSize -> BitVectorKind -> UnsignedSize -> SignedSize -> Bool
isSizeCompatible size Raw (UnSig minSize) _ = size >= minSize
isSizeCompatible size Unsigned (UnSig minSize) _ = size >= minSize
isSizeCompatible size Signed _ (Sig minSize) = size >= minSize

checkExpectedList :: Pos a -> ExpectedList -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpectedList err expL@(ExpectedList l) x = do
  v <- unfoldVal x
  case v of
    Nothing -> reportError err <$> "Type variable " <> showId x <> " is not " <> pure (show expL) <> "."
    Just k ->
      case checkAtom k of
        Nothing -> reportError err <$> "Unable to unify " <> showId x <> " with " <> pure (show expL) <> "."
        Just k' -> pure <$> setVal x k'
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

checkExpectedType :: Pos a -> AtomicTType -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpectedType err typ t = do
  v <- unfoldVal t
  case v of
    Nothing -> fromAtomicType err typ >>= (t ~>)
    Just k -> checkExpectedType' t typ k >>= embed . fmap (setVal t)
  where
    reportErr r = reportError err <$> "Cannot unify atomic type " <> showId r <> " with " <> pure (show typ) <> "."

    checkExpectedType' r ty k =
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
        then fmap (Custom tId1) . sequenceA <$> Monad.zipWithM (checkExpectedType err) args1 args2
        else reportErr r

checkExpected :: Pos a -> ExpectedType -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpected loc (EList expL) = checkExpectedList loc expL
checkExpected loc (EType eTyp) = checkExpectedType loc eTyp

buildAndBind :: Pos a -> TypeKind -> TypeUnifier TypeCand
buildAndBind loc v = do
  vId <- AtomicUnifier $ state mintNewCand
  setVal vId v
  where
    mintNewCand s =
      let cand@(TypeCand tInt) = nextId s
          incrTypeCand (TypeCand i) = TypeCand (i + 1)
          newNextId = incrTypeCand (nextId s)
          newTypCandPos = IntMap.insert tInt (loc $> ()) (typCandPos s)
       in (cand, s {nextId = newNextId, typCandPos = newTypCandPos})

constantTypeCand :: Pos a -> Constant -> TypeUnifier TypeCand
constantTypeCand loc = buildAndBind loc . go
  where
    go (BoolConst _) = Bool
    go (IntegerConst i) =
      let log2 = fromEnum (integerLog2 i)
          minUSize = UnSig (BVSize (1 + log2))
          minSSize = Sig (BVSize (2 + log2))
       in Numeric (Raw :| [Unsigned, Signed]) minUSize minSSize

fromAtomicType :: Pos a -> AtomicTType -> TypeUnifier TypeCand
fromAtomicType loc TBool = buildAndBind loc Bool
fromAtomicType loc (TBitVector kind size) = buildAndBind loc (BitVector kind size)
fromAtomicType loc (TCustom tId args) = mapM (fromAtomicType loc) args >>= buildAndBind loc . Custom tId

toAtom :: ExpectedList -> ExpectedType
toAtom = EList

fromType :: AtomicTType -> ExpectedType
fromType = EType

expectBitVector :: BitVectorKind -> ExpectedList
expectBitVector = ExpectedList . NonEmpty.singleton . UnsizedBitVector

expectBool :: ExpectedList
expectBool = ExpectedList (NonEmpty.singleton BoolExp)

instance MonadAssocReader TypeCand TypeKind (Reader StateUnifier)

runUnifier :: TypeUnifier a -> (a, CanFail (Map TypeCand AtomicTType))
runUnifier (AtomicUnifier m) = second buildMap $ runState m initialState
  where
    initialState = UState {unifState = emptyState, nextId = TypeCand 0, typCandPos = mempty}

foldMapM :: (Foldable t, Applicative f, Monoid b) => (a -> f b) -> t a -> f b
foldMapM f = getAp <$> foldMap (Ap . f)

buildMap :: UState -> CanFail (Map TypeCand AtomicTType)
buildMap st =
  let beg = TypeCand 0
      (TypeCand nextInt) = nextId st
      end = TypeCand $ nextInt - 1
   in sequenceA $ runReader (foldMapM go [beg .. end]) $ unifState st
  where
    go :: TypeCand -> Reader StateUnifier (Map TypeCand (CanFail AtomicTType))
    go t = getTyp t <&> Map.singleton t

    getTyp :: TypeCand -> Reader StateUnifier (CanFail AtomicTType)
    getTyp t@(TypeCand tInt) =
      let err = reportError ((IntMap.! tInt) (typCandPos st))
       in val t >>= maybe (return $ err "No type found for this expression.") (fromTypeKind err)

    fromTypeKind _ Bool = return $ pure TBool
    fromTypeKind _ (BitVector kind size) = return $ pure (TBitVector kind size)
    fromTypeKind err (Numeric {}) = return $ err "Unable to find the bit-vector size of this expression."
    fromTypeKind _ (Custom tId args) = fmap (TCustom tId) . sequenceA <$> mapM getTyp args