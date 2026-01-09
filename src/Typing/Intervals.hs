module Typing.Intervals
  ( IntervalResult (..),
    normalizeInterval,
  )
where

import Commons.Ast (Bound (..), Interval (..))
import Commons.Error (CanFail)
import Commons.Position (Pos (..))
import Commons.Size (SimpleSize, Size, splitSize)
import Control.Monad.Reader (Reader, runReader)
import Data.RatioInt (RatioInt)
import Parsing.Ast (SizeExpr)
import Typing.SizeEnv (SizeInfo, checkSizeExpr)

shiftInterval :: RatioInt -> Interval -> Interval
shiftInterval k (MinoredBy lo) = MinoredBy $ (+ k) <$> lo
shiftInterval k (Between lo hi) = Between ((+ k) <$> lo) ((+ k) <$> hi)
shiftInterval k (MajoredBy hi) = MajoredBy $ (+ k) <$> hi

checkCriterion :: (Interval, SizeExpr) -> Reader SizeInfo (CanFail (Interval, SimpleSize))
checkCriterion (i, sExpr) = fmap go <$> checkSizeExpr sExpr
  where
    go :: Size -> (Interval, SimpleSize)
    go s =
      let (coefs, cst) = splitSize s in (shiftInterval (-cst) i, coefs)

data IntervalResult = IntervalResult
  { crit :: SimpleSize,
    interval :: Interval,
    holes :: [Interval]
  }

flipBound :: Bound a -> Bound a
flipBound (Ex x) = In x
flipBound (In x) = Ex x

computeHoles :: Interval -> [Interval]
computeHoles (MajoredBy x) = [MinoredBy $ flipBound x]
computeHoles (MinoredBy x) = [MajoredBy $ flipBound x]
computeHoles (Between lo hi) = computeHoles (MinoredBy lo) ++ computeHoles (MajoredBy hi)

normalizeInterval :: SizeInfo -> Pos (Interval, SizeExpr) -> CanFail IntervalResult
normalizeInterval sInfo l = do
  let m = checkCriterion $ unwrap l
  (origInt, criterion) <- runReader m sInfo
  let hs = computeHoles origInt
  return $ IntervalResult criterion origInt hs
