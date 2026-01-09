{-# LANGUAGE InstanceSigs #-}

module Commons.Error
  ( CanFail (),
    reportError,
    reportLocatedError,
    addError,
    runCanFail,
    collapse,
    embed,
    collapseA,
  )
where

import Commons.Position (Pos (), showErrorMessage)
import Data.Foldable1 (intercalate1)
import Data.List.NonEmpty (NonEmpty (..), nub)
import qualified Data.List.NonEmpty as NonEmpty

data CanFail a = Ok !a | Error !(NonEmpty (Pos String))

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

instance Monad CanFail where
  (>>=) :: CanFail a -> (a -> CanFail b) -> CanFail b
  (>>=) (Ok x) f = f x
  (>>=) (Error s) _ = Error s

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

{-# INLINEABLE reportError #-}
reportError :: Pos b -> String -> CanFail a
reportError pos err = Error . NonEmpty.singleton $ err <$ pos

{-# INLINEABLE reportLocatedError #-}
reportLocatedError :: Pos String -> CanFail a
reportLocatedError = Error . NonEmpty.singleton

{-# INLINEABLE addError #-}
addError :: CanFail b -> Pos c -> String -> CanFail a
addError (Ok _) pos err = reportError pos err
addError (Error s) pos err = Error $ NonEmpty.cons (err <$ pos) s

runCanFail :: FilePath -> CanFail a -> Either String a
runCanFail fileName (Error s) =
  let msgs = nub $ showErrorMessage fileName <$> s
   in Left $ intercalate1 "\n" msgs
runCanFail _ (Ok x) = Right x
