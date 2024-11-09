{-# LANGUAGE InstanceSigs #-}

module Typing.TypeError
  ( TypeErrorDesc (),
    TypeError (..),
    CanFail (),
    reportError,
    addError,
    runCanFail,
    withError,
    canFailMapM,
    collapse,
    hasFailed,
  )
where

import Commons.AstTypes
import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Lazy (Text)
import Text.Megaparsec.Error
import Text.Megaparsec.State (initialPosState)
import Typing.Ast (TType)

data Err = Err Int TypeErrorDesc
  deriving (Eq, Ord)

data CanFail a = Ok a | Error (Set Err)

instance Functor CanFail where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> CanFail a -> CanFail b
  fmap f (Ok x) = Ok $ f x
  fmap _ (Error s) = Error s

instance Applicative CanFail where
  {-# INLINEABLE pure #-}
  pure :: a -> CanFail a
  pure = Ok

  {-# INLINEABLE (<*>) #-}
  (<*>) :: CanFail (a -> b) -> CanFail a -> CanFail b
  (<*>) (Ok f) (Ok x) = Ok $ f x
  (<*>) (Error s) (Ok _) = Error s
  (<*>) (Ok _) (Error s) = Error s
  (<*>) (Error s) (Error s') = Error $ s <> s'

  {-# INLINEABLE liftA2 #-}
  liftA2 :: (a -> b -> c) -> CanFail a -> CanFail b -> CanFail c
  liftA2 f (Ok a) (Ok b) = Ok $ f a b
  liftA2 _ (Error s) (Ok _) = Error s
  liftA2 _ (Ok _) (Error s) = Error s
  liftA2 _ (Error s) (Error s') = Error $ s <> s'

instance (Semigroup a) => Semigroup (CanFail a) where
  (<>) :: (Semigroup a) => CanFail a -> CanFail a -> CanFail a
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (CanFail a) where
  mempty :: (Monoid a) => CanFail a
  mempty = pure mempty

instance Foldable CanFail where
  foldMap :: (Monoid m) => (a -> m) -> CanFail a -> m
  foldMap = withError mempty

instance Traversable CanFail where
  traverse :: (Applicative f) => (a -> f b) -> CanFail a -> f (CanFail b)
  traverse f (Ok x) = Ok <$> f x
  traverse _ (Error s) = pure $ Error s

{-# INLINEABLE withError #-}
withError :: b -> (a -> b) -> CanFail a -> b
withError _ f (Ok x) = f x
withError x _ (Error _) = x

{-# INLINEABLE canFailMapM #-}
canFailMapM :: (Traversable t, Monad m) => (a -> m (CanFail b)) -> t a -> m (CanFail (t b))
canFailMapM f l = sequenceA <$> mapM f l

{-# INLINEABLE collapse #-}
collapse :: CanFail (CanFail a) -> CanFail a
collapse (Ok x) = x
collapse (Error s) = Error s

data TypeError
  = NodeAlreadyDeclared Ident
  | VariableAlreadyDeclared Ident
  | UnknownNode Ident
  | UnknownVariable Ident
  | InvalidType {expectedTyp :: TType, foundTyp :: TType, contextExpected :: TType, contextFound :: TType}
  | NotSameType {t1Typ :: TType, t2Typ :: TType, contextT1 :: TType, contextT2 :: TType}
  | MissingArgument {expectedArgs :: Int, foundArgs :: Int}
  | TooManyArguments {expectedArgs :: Int, foundArgs :: Int}
  | UnExpectedTuple TType
  | MissingExpression {expectedExpr :: Int, foundExpr :: Int}
  | TooManyExpressions {expectedExpr :: Int, foundExpr :: Int}
  deriving (Show, Eq, Ord)

data TypeErrorDesc = TypeErrorDesc TypeError Int
  deriving (Eq, Ord)

{-# INLINEABLE reportError #-}
reportError :: Localized a -> TypeError -> CanFail b
reportError (L pos _ end) err = Error . S.singleton . Err pos $ TypeErrorDesc err (end - pos)

{-# INLINEABLE addError #-}
addError :: CanFail a -> Localized b -> TypeError -> CanFail c
addError (Ok _) pos err = reportError pos err
addError (Error s) (L pos _ end) err = Error $ S.insert (Err pos (TypeErrorDesc err (end - pos))) s

{-# INLINEABLE hasFailed #-}
hasFailed :: CanFail a
hasFailed = Error S.empty

runCanFail :: FilePath -> Text -> CanFail a -> Either (ParseErrorBundle Text TypeErrorDesc) a
runCanFail fileName input (Error s) = Left $ ParseErrorBundle errList (initialPosState fileName input)
  where
    errList = case S.toList s of
      [] -> error "Failure Without Error"
      x : l -> toError <$> x :| l
    toError (Err pos desc) = FancyError pos . S.singleton . ErrorCustom $ desc
runCanFail _ _ (Ok x) = Right x

instance ShowErrorComponent TypeErrorDesc where
  showErrorComponent :: TypeErrorDesc -> String
  showErrorComponent (TypeErrorDesc t _) = show t

  errorComponentLen :: TypeErrorDesc -> Int
  errorComponentLen (TypeErrorDesc _ l) = l