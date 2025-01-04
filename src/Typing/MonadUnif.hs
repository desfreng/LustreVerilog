{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Typing.MonadUnif
  ( UnifState (),
    ConvertibleToInt (..),
    MonadAssocReader (val),
    MonadUnif (unifyCand, subVars, idName, showId, setVal, setRepr, unfoldVal, unify, (~>)),
    emptyState,
  )
where

import Commons.Position (Pos)
import Commons.TypingError (CanFail, collapseA, reportError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State.Strict (MonadState (state), gets)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldr (orM . f) (pure False)

orM :: (Monad m) => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

newtype UnifState id cand = UnifState {idEquiv :: IntMap (Either id cand)}
  deriving (Show)

emptyState :: UnifState id cand
emptyState = UnifState IMap.empty

class ConvertibleToInt a where
  toInt :: a -> Int

class (Eq id, ConvertibleToInt id, MonadReader (UnifState id cand) m) => MonadAssocReader id cand m | id -> cand, id -> m where
  val :: id -> m (Maybe cand)
  val x = do
    binding <- asks (IMap.lookup (toInt x) . idEquiv)
    case binding of
      Nothing -> return Nothing
      Just (Right v) -> return $ Just v
      Just (Left t) -> val t

class (Eq id, ConvertibleToInt id, MonadState (UnifState id cand) m) => MonadUnif id cand m | id -> cand, id -> m where
  unifyCand :: Pos a -> (id, cand) -> (id, cand) -> m (CanFail cand)
  subVars :: cand -> [id]
  idName :: m String
  showId :: id -> m String

  setRepr :: id -> id -> m id
  setRepr x r = do
    rx <- getRepr x
    if rx == r
      then return r -- Invariant, no self equivalence in varEquiv !
      else state $ \(UnifState s) -> (r, UnifState (IMap.insert (toInt rx) (Left r) s))

  setVal :: id -> cand -> m id
  setVal x v = do
    r <- getRepr x
    state $ \(UnifState s) -> (r, UnifState (IMap.insert (toInt r) (Right v) s))

  getBinding :: id -> m (Maybe (Either id cand))
  getBinding x = gets (IMap.lookup (toInt x) . idEquiv)

  getRepr :: id -> m id
  getRepr x = do
    binding <- getBinding x
    case binding of
      Nothing -> return x
      Just (Right _) -> return x
      Just (Left t) -> getRepr t >>= setRepr x

  getVal :: id -> m (Maybe cand)
  getVal x = do
    binding <- getBinding x
    case binding of
      Nothing -> return Nothing
      Just (Left t) -> getVal t
      Just (Right t) -> return (Just t)

  unfoldVal :: id -> m (Maybe cand)
  unfoldVal v = getRepr v >>= getVal

  (~>) :: id -> id -> m (CanFail id)
  x ~> r = pure <$> setRepr x r

  unify :: Pos a -> id -> id -> m (CanFail id)
  unify loc x1 x2 = do
    r1 <- getRepr x1
    r2 <- getRepr x2
    s1 <- getVal r1
    s2 <- getVal r2
    if r1 == r2
      then return $ pure r1
      else case (s1, s2) of
        (Nothing, Nothing) -> r2 ~> r1
        (Just _, Nothing) -> r2 ~&> r1
        (Nothing, Just _) -> r1 ~&> r2
        (Just t1, Just t2) -> unifyCand loc (r1, t1) (r2, t2) >>= collapseA . fmap (\v -> setVal r1 v >> r2 ~> r1)
    where
      (~&>) x r = do
        xInR <- occurs x r
        if xInR
          then do
            n <- idName
            ppX <- showId x
            ppR <- showId r
            return $ reportError loc $ "The " <> n <> " variable " <> ppX <> " appears in the " <> n <> " " <> ppR <> "."
          else x ~> r

      occurs v y = do
        r2 <- getRepr y
        v2 <- getVal r2
        if v == r2
          then return True
          else case v2 of
            Nothing -> return False
            Just t -> anyM (occurs v) (subVars t)
