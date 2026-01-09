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
    getSize,
    getFixedSize,
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
    -- Size Comparision
    isStrictlySmaller,
    isSmaller,
    isEqual,
    checkSizeExpr,
    checkSizeInDecl,
    checkSizeConstraint,
    SystemResult,
    solveSystem,
  )
where

import Commons.Ast (Constant (..), NodeSignature, SizeConstraint)
import Commons.Error (CanFail, embed, reportError)
import Commons.Position (Pos)
import Commons.Size (Size, constantSize, subSize)
import Commons.Types (AtomicTType (..), BitVectorKind (..))
import Control.Monad.Reader (Reader, ReaderT, runReader, runReaderT)
import Control.Monad.State.Strict (MonadState (state), State, runState)
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
import Data.Maybe (mapMaybe)
import Data.Monoid (Ap (Ap, getAp))
import Data.RatioInt ((%))
import Data.String (IsString (..))
import Parsing.Ast (SizeExpr)
import Typing.MonadUnif (ConvertibleToInt (..), MonadAssocReader (..), MonadUnif (..), UnifState, emptyState)
import Typing.SizeEnv (SizeInfo, SystemResult)
import qualified Typing.SizeEnv as S

{-# INLINEABLE ifM #-}
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM b t f = do bb <- b; if bb then t else f

findM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

foldMapM :: (Foldable t, Applicative f, Monoid b) => (a -> f b) -> t a -> f b
foldMapM f = getAp <$> foldMap (Ap . f)

newtype UnsignedSize = UnSig Int
  deriving (Eq, Ord)

newtype SignedSize = Sig Int
  deriving (Eq, Ord)

data TypeKind
  = BitVector BitVectorKind Size
  | Numeric (NonEmpty BitVectorKind) UnsignedSize SignedSize
  | Bool

showNumeric :: NonEmpty BitVectorKind -> UnsignedSize -> SignedSize -> String
showNumeric numKind uS sS = intercalate "|" . NonEmpty.toList $ showNumKind uS sS <$> numKind
  where
    showNumKind :: UnsignedSize -> SignedSize -> BitVectorKind -> String
    showNumKind (UnSig s) _ Raw = "r(>=" <> show s <> ")"
    showNumKind (UnSig s) _ Unsigned = "u(>=" <> show s <> ")"
    showNumKind _ (Sig s) Signed = "i(>=" <> show s <> ")"

data TypeCand = TypeCand Int String
  deriving (Show)

instance Eq TypeCand where
  (==) :: TypeCand -> TypeCand -> Bool
  (TypeCand x _) == (TypeCand y _) = x == y

instance Ord TypeCand where
  compare (TypeCand x _) (TypeCand y _) = compare x y

instance Enum TypeCand where
  toEnum :: Int -> TypeCand
  toEnum i = TypeCand i ""

  fromEnum :: TypeCand -> Int
  fromEnum (TypeCand i _) = i

instance ConvertibleToInt TypeCand where
  toInt :: TypeCand -> Int
  toInt (TypeCand i _) = i

data ExpectedKind = BoolExp | UnsizedBitVector BitVectorKind
  deriving (Eq)

instance Show ExpectedKind where
  show :: ExpectedKind -> String
  show BoolExp = "bool"
  show (UnsizedBitVector Raw) = "r??"
  show (UnsizedBitVector Unsigned) = "u??"
  show (UnsizedBitVector Signed) = "i??"

newtype ExpectedList = ExpectedList (NonEmpty ExpectedKind)

instance Semigroup ExpectedList where
  (<>) :: ExpectedList -> ExpectedList -> ExpectedList
  (ExpectedList a) <> (ExpectedList b) = ExpectedList $ a <> b

data ExpectedType = EList ExpectedList | EType AtomicTType

instance Show ExpectedList where
  show :: ExpectedList -> String
  show (ExpectedList atom) = intercalate "|" (NonEmpty.toList $ show <$> atom)

instance Show ExpectedType where
  show :: ExpectedType -> String
  show (EList l) = show l
  show (EType typ) = show typ

type StateUnifier = UnifState TypeCand TypeKind

data UState = UState {unifState :: StateUnifier, nextId :: Int, typCandPos :: IntMap (Pos ())}

newtype TypeUnifier a = TypeUnifier (ReaderT SizeInfo (State UState) a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (TypeUnifier a) where
  (<>) :: TypeUnifier a -> TypeUnifier a -> TypeUnifier a
  (<>) = liftA2 (<>)

instance (IsString a) => IsString (TypeUnifier a) where
  fromString :: (IsString a) => String -> TypeUnifier a
  fromString = pure . fromString

instance MonadState StateUnifier TypeUnifier where
  state :: (UnifState TypeCand TypeKind -> (a, UnifState TypeCand TypeKind)) -> TypeUnifier a
  state f = TypeUnifier . state $ \s -> let (x, s') = f (unifState s) in (x, s {unifState = s'})

instance MonadUnif TypeCand TypeKind TypeUnifier where
  {-# INLINEABLE subVars #-}
  subVars :: TypeKind -> [TypeCand]
  subVars Bool = []
  subVars BitVector {} = []
  subVars Numeric {} = []

  {-# INLINEABLE idName #-}
  idName :: TypeUnifier String
  idName = "atomic type"

  showId :: TypeCand -> TypeUnifier String
  showId t = unfoldVal t >>= go
    where
      go :: Maybe TypeKind -> TypeUnifier String
      go Nothing = return "?"
      go (Just x) = showAtom' x

      showAtom' :: TypeKind -> TypeUnifier String
      showAtom' Bool = return "bool"
      showAtom' (BitVector k s) = return $ show (TBitVector k s)
      showAtom' (Numeric numKind uS sS) = return $ showNumeric numKind uS sS

  unifyCand ::
    Pos a -> (TypeCand, TypeKind) -> (TypeCand, TypeKind) -> TypeUnifier (CanFail TypeKind)
  unifyCand err (r1@(TypeCand _ ctx1), t1) (r2@(TypeCand _ ctx2), t2) =
    case (t1, t2) of
      -- Bool
      (Bool, Bool) -> return $ pure Bool
      (Bool, BitVector Raw s) -> unifyBoolRaw s
      (Bool, BitVector {}) -> reportErr
      (Bool, Numeric {}) -> reportErr
      -- BitVector
      (BitVector Raw s, Bool) -> unifyBoolRaw s
      (BitVector {}, Bool) -> reportErr
      (BitVector kind1 size1, BitVector kind2 size2) -> unifyBV kind1 kind2 size1 size2
      (BitVector kind size, Numeric numKind minUSize minSSize) -> unifyBVNum kind size numKind minUSize minSSize
      -- Numeric
      (Numeric {}, Bool) -> reportErr
      (Numeric k1 uS1 sS1, Numeric k2 uS2 sS2) -> unifyNumNum k1 uS1 sS1 k2 uS2 sS2
      (Numeric numKind minUSize minSSize, BitVector kind size) -> unifyBVNum kind size numKind minUSize minSSize
    where
      reportErr :: TypeUnifier (CanFail a)
      reportErr =
        let c1 = if ctx1 /= "" then "\nContext for " <> showId r1 <> ": " <> pure ctx1 else ""
            c2 = if ctx2 /= "" then "\nContext for " <> showId r2 <> ": " <> pure ctx2 else ""
         in reportError err <$> "Cannot unify atomic type " <> showId r1 <> " with " <> showId r2 <> "." <> c1 <> c2

      unifyBV :: BitVectorKind -> BitVectorKind -> Size -> Size -> TypeUnifier (CanFail TypeKind)
      unifyBV kind1 kind2 size1 size2 = do
        isEq <- size1 `isEqual` size2
        if kind1 == kind2 && isEq
          then return $ pure t1
          else reportErr

      unifyBoolRaw :: Size -> TypeUnifier (CanFail TypeKind)
      unifyBoolRaw s = do
        isEq <- s `isEqual` constantSize 1
        if isEq then return $ pure Bool else reportErr

      unifyBVNum :: BitVectorKind -> Size -> NonEmpty BitVectorKind -> UnsignedSize -> SignedSize -> TypeUnifier (CanFail TypeKind)
      unifyBVNum kind size numKind minUSize minSSize = do
        isGood <- isSizeCompatible size kind minUSize minSSize
        if kind `elem` numKind && isGood
          then return $ pure (BitVector kind size)
          else reportErr

      unifyNumNum :: NonEmpty BitVectorKind -> UnsignedSize -> SignedSize -> NonEmpty BitVectorKind -> UnsignedSize -> SignedSize -> TypeUnifier (CanFail TypeKind)
      unifyNumNum k1 uS1 sS1 k2 uS2 sS2 =
        case filterKind k1 (NonEmpty.toList k2) of
          Just k -> return . pure $ Numeric k (max uS1 uS2) (max sS1 sS2)
          Nothing -> reportErr

unifyTypeCand :: Pos a -> TypeCand -> TypeCand -> TypeUnifier (CanFail TypeCand)
unifyTypeCand = unify

showTypeCand :: TypeCand -> TypeUnifier String
showTypeCand = showId

filterKind :: NonEmpty BitVectorKind -> [BitVectorKind] -> Maybe (NonEmpty BitVectorKind)
filterKind l1 l2 = NonEmpty.nonEmpty (NonEmpty.toList l1 `intersect` l2)

checkSizeExpr :: SizeExpr -> TypeUnifier (CanFail Size)
checkSizeExpr e = TypeUnifier $ S.checkSizeExpr e

checkSizeInDecl :: SizeExpr -> TypeUnifier (CanFail Size)
checkSizeInDecl e = TypeUnifier $ S.checkSizeInDecl e

-- | x `isStrictlySmaller` y checks that x < y
isStrictlySmaller :: Size -> Size -> TypeUnifier Bool
isStrictlySmaller x y = TypeUnifier $ S.isStrictlySmaller x y

-- | x `isSmaller` y checks that x <= y
isSmaller :: Size -> Size -> TypeUnifier Bool
isSmaller x y = TypeUnifier $ S.isSmaller x y

isEqual :: Size -> Size -> TypeUnifier Bool
isEqual x y = TypeUnifier $ S.isZero (subSize x y)

checkSizeConstraint :: SizeConstraint Size -> TypeUnifier Bool
checkSizeConstraint = TypeUnifier . S.checkSizeConstraint

solveSystem :: Pos a -> NodeSignature -> [Pos (Size, Size)] -> TypeUnifier (CanFail SystemResult)
solveSystem loc nSig = TypeUnifier . S.solveSystem loc nSig

isSizeCompatible :: Size -> BitVectorKind -> UnsignedSize -> SignedSize -> TypeUnifier Bool
isSizeCompatible size Raw (UnSig minSize) _ = constantSize (minSize % 1) `isSmaller` size
isSizeCompatible size Unsigned (UnSig minSize) _ = constantSize (minSize % 1) `isSmaller` size
isSizeCompatible size Signed _ (Sig minSize) = constantSize (minSize % 1) `isSmaller` size

checkExpectedList :: Pos a -> ExpectedList -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpectedList err expL@(ExpectedList l) x@(TypeCand _ ctx) = do
  v <- unfoldVal x
  case v of
    Nothing ->
      let c = if ctx /= "" then "\nContext for " <> showId x <> ": " <> pure ctx else ""
       in reportError err <$> "Type variable " <> showId x <> " is not " <> pure (show expL) <> "." <> c
    Just k -> do
      res <- checkAtom k
      case res of
        Nothing ->
          let c = if ctx /= "" then "\nContext for " <> showId x <> ": " <> pure ctx else ""
           in reportError err <$> "Unable to unify " <> showId x <> " with " <> pure (show expL) <> "." <> c
        Just k' -> pure <$> setVal x k'
  where
    checkAtom :: TypeKind -> TypeUnifier (Maybe TypeKind)
    checkAtom v@Bool = return $ find (== BoolExp) l $> v
    checkAtom v@(BitVector kind size) = do
      res <- findM (isCompatibleBitVector kind size) l
      return $ res $> v
    checkAtom (Numeric nK uS sS) =
      let buildNumeric u s k = Numeric k u s
       in return $ buildNumeric uS sS <$> filterKind nK (mapMaybe getBitVectorKind (NonEmpty.toList l))

    isCompatibleBitVector :: BitVectorKind -> Size -> ExpectedKind -> TypeUnifier Bool
    isCompatibleBitVector _ s BoolExp = s `isEqual` constantSize 1
    isCompatibleBitVector kind _ (UnsizedBitVector expKind) = return $ kind == expKind

    getBitVectorKind :: ExpectedKind -> Maybe BitVectorKind
    getBitVectorKind BoolExp = Nothing
    getBitVectorKind (UnsizedBitVector k) = Just k

checkExpectedType :: Pos a -> AtomicTType -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpectedType err typ t = do
  v <- unfoldVal t
  case v of
    Nothing -> fromAtomicType err "" typ >>= (t ~>)
    Just k -> checkExpectedType' t typ k >>= embed . fmap (setVal t)
  where
    reportErr :: TypeCand -> TypeUnifier (CanFail a)
    reportErr r@(TypeCand _ ctx) =
      let c = if ctx /= "" then "\nContext for " <> showId r <> ": " <> pure ctx else ""
       in reportError err <$> "Cannot unify atomic type " <> showId r <> " with " <> pure (show typ) <> "." <> c

    checkExpectedType' :: TypeCand -> AtomicTType -> TypeKind -> TypeUnifier (CanFail TypeKind)
    checkExpectedType' r ty k =
      case (ty, k) of
        -- TBool
        (TBool, Bool) -> return $ pure Bool
        (TBool, BitVector Raw s) -> unifyBoolRaw s r
        (TBool, BitVector {}) -> reportErr r
        (TBool, Numeric {}) -> reportErr r
        -- TBitVector
        (TBitVector Raw s, Bool) -> unifyBoolRaw s r
        (TBitVector {}, Bool) -> reportErr r
        (TBitVector typKind typSize, BitVector candKind candSize) -> unifyBV r typKind typSize candKind candSize
        (TBitVector typKind typSize, Numeric numKind uS sS) -> unifyBVNum r typKind typSize numKind uS sS

    unifyBoolRaw :: Size -> TypeCand -> TypeUnifier (CanFail TypeKind)
    unifyBoolRaw s r = do
      isEq <- s `isEqual` constantSize 1
      if isEq then return $ pure Bool else reportErr r

    unifyBV :: TypeCand -> BitVectorKind -> Size -> BitVectorKind -> Size -> TypeUnifier (CanFail TypeKind)
    unifyBV r typKind typSize candKind candSize =
      do
        eqSize <- typSize `isEqual` candSize
        if typKind == candKind && eqSize
          then return $ pure (BitVector typKind typSize)
          else reportErr r

    unifyBVNum :: TypeCand -> BitVectorKind -> Size -> NonEmpty BitVectorKind -> UnsignedSize -> SignedSize -> TypeUnifier (CanFail TypeKind)
    unifyBVNum r typKind typSize numKind uS sS = do
      isGood <- isSizeCompatible typSize typKind uS sS
      if typKind `elem` numKind && isGood
        then return $ pure (BitVector typKind typSize)
        else reportErr r

getSize :: TypeCand -> TypeUnifier (Either TypeCand Size)
getSize t = do
  v <- unfoldVal t
  case v of
    Nothing -> return $ Left t
    Just (BitVector _ x) -> return $ Right x
    Just Bool -> return $ Right $ constantSize 1
    Just (Numeric {}) -> return $ Left t

getFixedSize :: Pos a -> TypeCand -> TypeUnifier (CanFail Size)
getFixedSize err t = do
  s <- getSize t
  case s of
    Left _ -> reportErr
    Right res -> return $ pure res
  where
    reportErr :: TypeUnifier (CanFail a)
    reportErr = reportError err <$> "The type candidate " <> showId t <> " does not have a fixed size."

checkExpected :: Pos a -> ExpectedType -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpected loc (EList expL) = checkExpectedList loc expL
checkExpected loc (EType eTyp) = checkExpectedType loc eTyp

buildAndBind :: Pos a -> String -> TypeKind -> TypeUnifier TypeCand
buildAndBind loc ctx v = do
  vId <- TypeUnifier $ state mintNewCand
  setVal vId v
  where
    mintNewCand :: UState -> (TypeCand, UState)
    mintNewCand s =
      let tInt = nextId s
          newNextId = tInt + 1
          newTypCandPos = IntMap.insert tInt (loc $> ()) (typCandPos s)
       in (TypeCand tInt ctx, s {nextId = newNextId, typCandPos = newTypCandPos})

constantTypeCand :: Pos a -> Constant -> TypeUnifier TypeCand
constantTypeCand loc = buildAndBind loc "" . go
  where
    busSize :: Integer -> Int
    busSize 0 = 0
    busSize x = floor . logBase (2.0 :: Double) $ fromIntegral x

    go (BoolConst _) = Bool
    go (IntegerConst i) =
      let log2 = fromEnum (busSize i)
          minUSize = UnSig (1 + log2)
          minSSize = Sig (2 + log2)
       in Numeric (Raw :| [Unsigned, Signed]) minUSize minSSize

fromAtomicType :: Pos a -> String -> AtomicTType -> TypeUnifier TypeCand
fromAtomicType loc ctx TBool = buildAndBind loc ctx Bool
fromAtomicType loc ctx (TBitVector kind size) = buildAndBind loc ctx (BitVector kind size)

toAtom :: ExpectedList -> ExpectedType
toAtom = EList

fromType :: AtomicTType -> ExpectedType
fromType = EType

expectBitVector :: BitVectorKind -> ExpectedList
expectBitVector = ExpectedList . NonEmpty.singleton . UnsizedBitVector

expectBool :: ExpectedList
expectBool = ExpectedList (NonEmpty.singleton BoolExp)

instance MonadAssocReader TypeCand TypeKind (Reader StateUnifier)

runUnifier :: SizeInfo -> TypeUnifier a -> (a, CanFail (Map TypeCand AtomicTType))
runUnifier sInfo (TypeUnifier m) = second buildMap $ runState (runReaderT m sInfo) initialState
  where
    initialState :: UState
    initialState = UState {unifState = emptyState, nextId = 0, typCandPos = mempty}

buildMap :: UState -> CanFail (Map TypeCand AtomicTType)
buildMap st =
  let beg = TypeCand 0 ""
      nextInt = nextId st
      end = TypeCand (nextInt - 1) ""
   in sequenceA $ runReader (foldMapM go [beg .. end]) $ unifState st
  where
    go :: TypeCand -> Reader StateUnifier (Map TypeCand (CanFail AtomicTType))
    go t = getTyp t <&> Map.singleton t

    getTyp :: TypeCand -> Reader StateUnifier (CanFail AtomicTType)
    getTyp t@(TypeCand tInt _) =
      let err = reportError ((IntMap.! tInt) (typCandPos st))
       in val t >>= maybe (return $ err "No type found for this expression.") (fromTypeKind err)

    fromTypeKind :: (String -> CanFail AtomicTType) -> TypeKind -> Reader StateUnifier (CanFail AtomicTType)
    fromTypeKind _ Bool = return $ pure TBool
    fromTypeKind _ (BitVector kind size) = return $ pure (TBitVector kind size)
    fromTypeKind err (Numeric kind uS sS) = return . err $ "Unable to find the bit-vector size of this expression. Type: " <> showNumeric kind uS sS <> "."
