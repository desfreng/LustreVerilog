{-# LANGUAGE InstanceSigs #-}

module Commons.TypingError
  ( TypingError (),
    CanFail (),
    reportError,
    addError,
    runCanFail,
    collapse,
    embed,
    collapseA,
  )
where

import Commons.Position (Pos (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text.Lazy (Text)
import Text.Megaparsec.Error
import Text.Megaparsec.State (initialPosState)

data Err = Err Int TypingError
  deriving (Eq, Ord)

data CanFail a = Ok a | Error (NonEmpty Err)

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

{-# INLINEABLE collapse #-}
collapse :: CanFail (CanFail a) -> CanFail a
collapse (Ok x) = x
collapse (Error s) = Error s

{-# INLINEABLE embed #-}
embed :: (Applicative f) => CanFail (f a) -> f (CanFail a)
embed (Ok x) = Ok <$> x
embed (Error l) = pure $ Error l

{-# INLINEABLE collapseA #-}
collapseA :: (Applicative f) => CanFail (f (CanFail a)) -> f (CanFail a)
collapseA = fmap collapse . embed

data TypingError = TypingError !String !Int
  deriving (Eq, Ord)

{-# INLINEABLE reportError #-}
reportError :: Pos b -> String -> CanFail a
reportError (L pos _ end) err = Error . NonEmpty.singleton . Err pos $ TypingError err (end - pos)

{-# INLINEABLE addError #-}
addError :: CanFail b -> Pos c -> String -> CanFail a
addError (Ok _) pos err = reportError pos err
addError (Error s) (L pos _ end) err = Error $ NonEmpty.cons (Err pos (TypingError err (end - pos))) s

runCanFail :: FilePath -> Text -> CanFail a -> Either (ParseErrorBundle Text TypingError) a
runCanFail fileName input (Error s) = Left $ ParseErrorBundle errList (initialPosState fileName input)
  where
    errList = toError <$> (NonEmpty.sortBy cmpErr $ NonEmpty.nub s)
    cmpErr (Err x _) (Err y _) = compare x y
    toError (Err pos desc) = FancyError pos . Set.singleton . ErrorCustom $ desc
runCanFail _ _ (Ok x) = Right x

instance ShowErrorComponent TypingError where
  showErrorComponent :: TypingError -> String
  showErrorComponent (TypingError t _) = t

  errorComponentLen :: TypingError -> Int
  errorComponentLen (TypingError _ l) = l
