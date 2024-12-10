{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Typing.Unification
  ( -- TypeUnifier Monad
    TypeUnifier (),
    runTypeUnifier,
    -- TypeCandidate type
    TypeCand (),
    constantTypeCand,
    tupleTypeCand,
    fromAtomicType,
    nodeOutputTypeCand,
    unifyTypeCand,
    unifyWithTType,
    -- ExpectedAtom
    ExpectedAtom (),
    expectBitVector,
    expectBool,
    checkExpected,
  )
where

import Commons.AstTypes
import Control.Applicative (Applicative (liftA2))
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable
import Data.Functor
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap
import Data.List (intercalate, intersect)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import GHC.Num (integerLog2)
import Typing.Ast (AtomicTType (..), NodeSignature (outputType), TType (..))
import Typing.TypeError (CanFail, ErrorReporter, collapseA, reportError)

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldr (orM . f) (pure False)

orM :: (Monad m) => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

newtype UnsignedSize = UnSig BVSize
  deriving (Show, Eq, Ord)

newtype SignedSize = Sig BVSize
  deriving (Show, Eq, Ord)

data AtomicCand
  = BitVector BitVectorKind BVSize
  | Numeric (NonEmpty BitVectorKind) UnsignedSize SignedSize
  | BoolCand
  deriving (Show)

data TypeCandKind
  = Tuple (BiList TypeCand)
  | Atomic AtomicCand
  | Custom () [TypeCand]
  deriving (Show)

newtype TypeCand = TypeCand Int
  deriving (Show, Eq, Ord)

data ExpectedKind = BoolExp | UnsizedBitVector BitVectorKind
  deriving (Show, Eq)

newtype ExpectedAtom = ExpectedAtom (NonEmpty ExpectedKind)
  deriving (Show, Semigroup)

data UnifState = UnifState {idEquiv :: IntMap (Either TypeCand TypeCandKind), nextId :: TypeCand}
  deriving (Show)

newtype TypeUnifier a = TU (State UnifState a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (TypeUnifier a) where
  (<>) :: TypeUnifier a -> TypeUnifier a -> TypeUnifier a
  (<>) = liftA2 (<>)

instance IsString (TypeUnifier String) where
  fromString :: String -> TypeUnifier String
  fromString = pure

filterKind :: NonEmpty BitVectorKind -> [BitVectorKind] -> Maybe (NonEmpty BitVectorKind)
filterKind l1 l2 = NonEmpty.nonEmpty (NonEmpty.toList l1 `intersect` l2)

isSizeCompatible :: BVSize -> BitVectorKind -> UnsignedSize -> SignedSize -> Bool
isSizeCompatible size Raw (UnSig minSize) _ = size >= minSize
isSizeCompatible size Unsigned (UnSig minSize) _ = size >= minSize
isSizeCompatible size Signed _ (Sig minSize) = size >= minSize

repr :: TypeCand -> TypeUnifier (Maybe TypeCand)
repr (TypeCand x) = TU $ gets (go . IMap.lookup x . idEquiv)
  where
    go (Just (Left r)) = Just r
    go _ = Nothing

val :: TypeCand -> TypeUnifier (Maybe TypeCandKind)
val (TypeCand x) = TU $ gets (go . IMap.lookup x . idEquiv)
  where
    go (Just (Right v)) = Just v
    go _ = Nothing

setRepr :: TypeCand -> TypeCand -> TypeUnifier TypeCand
setRepr (TypeCand x) r@(TypeCand rId) =
  if x == rId
    then return r -- Invariant, no self equivalence in varEquiv !
    else
      TU $ modify (\s -> s {idEquiv = IMap.insert x (Left r) (idEquiv s)}) $> r

setVal :: TypeCand -> TypeCandKind -> TypeUnifier ()
setVal (TypeCand x) v = TU $ modify (\s -> s {idEquiv = IMap.insert x (Right v) (idEquiv s)})

unfold :: TypeCand -> TypeUnifier TypeCand
unfold v = do
  candidate <- repr v
  case candidate of
    Nothing -> return v
    Just c -> unfold c >>= setRepr v

(~>) :: (Applicative f) => TypeCand -> TypeCand -> TypeUnifier (f TypeCand)
(~>) x r = pure <$> setRepr x r

unifyTypeCand :: ErrorReporter -> TypeCand -> TypeCand -> TypeUnifier (CanFail TypeCand)
unifyTypeCand err x1 x2 = do
  r1 <- unfold x1
  r2 <- unfold x2
  s1 <- val r1
  s2 <- val r2
  if r1 == r2
    then return $ pure r1
    else case (s1, s2) of
      (Nothing, Nothing) -> r2 ~> r1
      (Just t, Nothing) -> unifyVar r2 (r1, t)
      (Nothing, Just t) -> unifyVar r1 (r2, t)
      (Just (Tuple l1), Just (Tuple l2)) -> unifyTuple (r1, l1) (r2, l2)
      (Just (Atomic bv1), Just (Atomic bv2)) ->
        case unifyAtom bv1 bv2 of
          Nothing -> unifError r1 r2
          Just atom -> setVal r1 (Atomic atom) >> r2 ~> r1
      (Just (Custom _ _), Just (Custom _ _)) -> unifError r1 r2
      (Just _, Just _) -> unifError r1 r2
  where
    unifError t1 t2 =
      err <$> "Cannot unify the type " <> showErr t1 <> " with the type " <> showErr t2 <> "."

    unifyVar x (r, t) = case t of
      Atomic _ -> x ~> r
      Custom _ l -> unifyIfNoOccur x r l
      Tuple l -> unifyIfNoOccur x r l

    unifyIfNoOccur :: (Foldable t) => TypeCand -> TypeCand -> t TypeCand -> TypeUnifier (CanFail TypeCand)
    unifyIfNoOccur x r l = do
      xInR <- anyM (occurs x) l
      if xInR
        then
          err <$> "The type variable " <> showErr x <> " appears in the type " <> showErr r <> "."
        else
          x ~> r

    occurs x y = do
      r2 <- unfold y
      v2 <- val r2
      if x == r2
        then return True
        else case v2 of
          Nothing -> return False
          Just (Atomic _) -> return False
          Just (Custom _ l) -> anyM (occurs x) l
          Just (Tuple l) -> anyM (occurs x) l

    unifyTuple (r1, l1) (r2, l2) =
      let ll1 = length l1
          ll2 = length l2
       in if ll1 == ll2
            then
              mapM (uncurry (unifyTypeCand err)) (bzip l1 l2)
                >>= collapseA . fmap (\l -> setVal r1 (Tuple l) >> r2 ~> r1) . sequenceA
            else
              err
                <$> "Cannot unify tuples of different lengths: "
                  <> pure (show ll1)
                  <> " and "
                  <> pure (show ll2)
                  <> ".\nTypes are: "
                  <> showErr r1
                  <> " and "
                  <> showErr r2
                  <> "."

unifyAtom :: AtomicCand -> AtomicCand -> Maybe AtomicCand
unifyAtom t1 t2 = case (t1, t2) of
  (BoolCand, BoolCand) -> Just BoolCand
  (BitVector kind1 size1, BitVector kind2 size2) -> if kind1 == kind2 && size1 == size2 then Just t1 else Nothing
  (Numeric k1 uS1 sS1, Numeric k2 uS2 sS2) -> unifyNumNum k1 uS1 sS1 k2 uS2 sS2
  (BitVector kind size, Numeric numKind minUSize minSSize) -> unifyBVNum kind size numKind minUSize minSSize
  (Numeric numKind minUSize minSSize, BitVector kind size) -> unifyBVNum kind size numKind minUSize minSSize
  (_, _) -> Nothing
  where
    unifyBVNum kind size numKind minUSize minSSize =
      if kind `elem` numKind && isSizeCompatible size kind minUSize minSSize
        then
          Just (BitVector kind size)
        else Nothing

    unifyNumNum k1 uS1 sS1 k2 uS2 sS2 =
      let buildNumeric k = Numeric k (max uS1 uS2) (max sS1 sS2)
       in buildNumeric <$> filterKind k1 (NonEmpty.toList k2)

unifyWithTType :: ErrorReporter -> TType -> TypeCand -> TypeUnifier (CanFail TypeCand)
unifyWithTType err typ x = do
  r <- unfold x
  t <- val r
  case t of
    Nothing -> candFromTType typ >>= (r ~>)
    Just t' -> unifyTType r typ t'
  where
    candFromTType (TAtom a) = fromAtomicType a
    candFromTType (TTuple l) = mapM candFromTType l >>= tupleTypeCand

    unifyTType r ttyp@(TTuple typL) (Tuple candL) =
      let typLength = length typL
          candLength = length candL
       in if candLength == typLength
            then
              mapM (uncurry (unifyWithTType err)) (bzip typL candL)
                >>= collapseA . fmap (\l -> setVal r (Tuple l) $> pure r) . sequenceA
            else
              err
                <$> "Cannot unify tuples of different lengths: "
                  <> pure (show typLength)
                  <> " and "
                  <> pure (show candLength)
                  <> ".\nTypes are: "
                  <> showErr r
                  <> " and "
                  <> pure (showTyp ttyp)
                  <> "."
    unifyTType r ttyp@(TAtom typAtom) (Atomic candAtom) =
      case unifyTTypeAtom typAtom candAtom of
        Nothing -> unifyError r ttyp
        Just c -> setVal r (Atomic c) $> pure r
    unifyTType _ _ (Custom _ _) = err <$> "Custom type are not implemented yet."
    unifyTType r ttyp _ = unifyError r ttyp

    unifyError r ttyp =
      err <$> "Unable to unify " <> showErr r <> " with " <> pure (showTyp ttyp) <> "."

unifyTTypeAtom :: AtomicTType -> AtomicCand -> Maybe AtomicCand
unifyTTypeAtom TBool t@BoolCand = Just t
unifyTTypeAtom (TBitVector typKind typSize) t@(BitVector candKind candSize) =
  if typKind == candKind && typSize == candSize then Just t else Nothing
unifyTTypeAtom (TBitVector typKind typSize) (Numeric numKind uS sS) =
  if typKind `elem` numKind && isSizeCompatible typSize typKind uS sS
    then Just (BitVector typKind typSize)
    else Nothing
unifyTTypeAtom _ _ = Nothing

checkExpected :: ErrorReporter -> ExpectedAtom -> TypeCand -> TypeUnifier (CanFail TypeCand)
checkExpected err expAtom x = do
  r <- unfold x
  t <- val r
  case t of
    Nothing -> err <$> "Type variable " <> showErr r <> " is not " <> pure (showExpAtom expAtom) <> "."
    Just t' -> checkExpected' r t' expAtom
  where
    checkExpected' r (Tuple _) expA = checkError r expA
    checkExpected' r (Custom _ _) expA = checkError r expA
    checkExpected' r (Atomic atom) expA =
      case checkAtom atom expA of
        Nothing -> checkError r expA
        Just a -> setVal r (Atomic a) $> pure r

    checkError r expT =
      err <$> "Unable to unify " <> showErr r <> " with " <> pure (showExpAtom expT) <> "."

checkAtom :: AtomicCand -> ExpectedAtom -> Maybe AtomicCand
checkAtom t (ExpectedAtom l) =
  case t of
    BoolCand -> find (== BoolExp) l $> t
    BitVector kind size -> find (isCompatibleBitVector kind size) l $> t
    Numeric nK uS sS -> buildNumeric uS sS <$> filterKind nK (mapMaybe getBitVectorKind (NonEmpty.toList l))
  where
    isCompatibleBitVector _ _ BoolExp = False
    isCompatibleBitVector kind _ (UnsizedBitVector expKind) = kind == expKind

    getBitVectorKind BoolExp = Nothing
    getBitVectorKind (UnsizedBitVector k) = Just k

    buildNumeric uS sS nK = Numeric nK uS sS

buildAndBind :: TypeCandKind -> TypeUnifier TypeCand
buildAndBind v = do
  newVar <- TU $ state (\s -> (nextId s, s {nextId = incr (nextId s)}))
  setVal newVar v
  return newVar
  where
    incr (TypeCand x) = TypeCand (x + 1)

constantTypeCand :: Constant -> TypeUnifier TypeCand
constantTypeCand = buildAndBind . Atomic . go
  where
    go (BoolConst _) = BoolCand
    go (IntegerConst i) =
      let log2 = fromEnum (integerLog2 i)
          minUSize = UnSig (BVSize (1 + log2))
          minSSize = Sig (BVSize (2 + log2))
       in Numeric (Raw :| [Unsigned, Signed]) minUSize minSSize

fromAtomicType :: AtomicTType -> TypeUnifier TypeCand
fromAtomicType TBool = buildAndBind (Atomic BoolCand)
fromAtomicType (TBitVector kind size) = buildAndBind (Atomic (BitVector kind size))

nodeOutputTypeCand :: NodeSignature -> TypeUnifier TypeCand
nodeOutputTypeCand = go . outputType
  where
    go (TTuple l) = mapM go l >>= tupleTypeCand
    go (TAtom x) = fromAtomicType x

expectBitVector :: BitVectorKind -> ExpectedAtom
expectBitVector = ExpectedAtom . NonEmpty.singleton . UnsizedBitVector

expectBool :: ExpectedAtom
expectBool = ExpectedAtom (NonEmpty.singleton BoolExp)

tupleTypeCand :: BiList TypeCand -> TypeUnifier TypeCand
tupleTypeCand = buildAndBind . Tuple

runTypeUnifier :: TypeUnifier a -> (a, Localized TypeCand -> CanFail TType)
runTypeUnifier (TU m) = second sanitize $ runState m UnifState {nextId = TypeCand 0, idEquiv = IMap.empty}

sanitize :: UnifState -> Localized TypeCand -> CanFail TType
sanitize st t = eval (go $ unwrap t)
  where
    eval (TU m) = evalState m st
    go tCand = unfold tCand >>= val >>= sanitizeTypeCand

    sanitizeTypeCand Nothing = return . reportError t $ "No final type found."
    sanitizeTypeCand (Just (Tuple l)) = mapM go l <&> fmap TTuple . sequenceA
    sanitizeTypeCand (Just (Atomic atom)) = return $ TAtom <$> sanitizeAtom atom
    sanitizeTypeCand (Just (Custom _ _)) = return . reportError t $ "Custom type are not implemented yet."

    sanitizeAtom BoolCand = pure TBool
    sanitizeAtom (BitVector kind size) = pure (TBitVector kind size)
    sanitizeAtom (Numeric {}) = reportError t "Unable to find the bit-vector size of this expression."

showErr :: TypeCand -> TypeUnifier String
showErr t = unfold t >>= val >>= go
  where
    go Nothing = return "?"
    go (Just (Atomic atom)) = return $ showAtom atom
    go (Just (Custom _ l)) = "Custom " <> showTypList l
    go (Just (Tuple l)) = showTypList (biListToList l)

    showTypList l = do
      l' <- mapM showErr l
      return $ "(" <> intercalate ", " l' <> ")"

    showAtom BoolCand = "bool"
    showAtom (BitVector Raw (BVSize i)) = "r" <> show i
    showAtom (BitVector Unsigned (BVSize i)) = "u" <> show i
    showAtom (BitVector Signed (BVSize i)) = "i" <> show i
    showAtom (Numeric numKind uS sS) = intercalate "|" . NonEmpty.toList $ showNumKind uS sS <$> numKind

    showNumKind (UnSig (BVSize uS)) _ Raw = "r(>=" <> show uS <> ")"
    showNumKind (UnSig (BVSize uS)) _ Unsigned = "u(>=" <> show uS <> ")"
    showNumKind _ (Sig (BVSize sS)) Signed = "i(>=" <> show sS <> ")"

showExpAtom :: ExpectedAtom -> String
showExpAtom (ExpectedAtom atom) = intercalate "|" (NonEmpty.toList $ showAtom <$> atom)
  where
    showAtom BoolExp = "bool"
    showAtom (UnsizedBitVector Raw) = "r??"
    showAtom (UnsizedBitVector Unsigned) = "u??"
    showAtom (UnsizedBitVector Signed) = "i??"

showTyp :: TType -> String
showTyp (TTuple l) = "(" <> intercalate ", " (biListToList $ showTyp <$> l) <> ")"
showTyp (TAtom TBool) = "bool"
showTyp (TAtom (TBitVector Raw (BVSize s))) = "r" <> show s
showTyp (TAtom (TBitVector Unsigned (BVSize s))) = "u" <> show s
showTyp (TAtom (TBitVector Signed (BVSize s))) = "i" <> show s
