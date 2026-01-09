{-# LANGUAGE InstanceSigs #-}

module Commons.Size
  ( Size,
    SimpleSize,
    constantSize,
    varSize,
    scaleSize,
    sumSize,
    subSize,
    sumVar,
    subVar,
    subst,
    hasVar,
    isNull,
    isCollinear,
    showRatio,
    prettyRatio,
    splitSize,
    getCoeffs,
    toSize,
  )
where

import Commons.Ids (SizeIdent)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.RatioInt (RatioInt (..))
import Prettyprinter (Doc, Pretty (..), unsafeViaShow)
import Solver.VectorSpace (VectorSpace (..))
import Text.Printf (printf)

showMag :: RatioInt -> String -> String
showMag r v =
  let n = numerator r
      d = denominator r
   in if d == 1 then printf "%d*%s" n v else printf "%d*%s/%d" n v d

formatTerms :: (Bool, String) -> String
formatTerms (True, s) = " - " <> s
formatTerms (False, s) = " + " <> s

cleanSum :: [(Bool, String)] -> [String]
cleanSum [] = ["0"]
cleanSum ((False, s) : rest) = s : (formatTerms <$> rest)
cleanSum ((True, s) : rest) = ("-" <> s) : (formatTerms <$> rest)

coefStr :: (Show k) => Map k RatioInt -> [(Bool, String)]
coefStr coef = do
  (v, c) <- M.toList coef
  let sign = c < 0
  let mag = abs c
  let val = if mag == 1 then show v else showMag mag $ show v
  return (sign, val)

constantStr :: RatioInt -> [(Bool, String)]
constantStr c
  | c == 0 = []
  | otherwise =
      let sign = c < 0
          mag = abs c
       in if denominator mag == 1
            then [(sign, show $ numerator mag)]
            else [(sign, printf "%d/%d" (numerator mag) (denominator mag))]

showRatio :: RatioInt -> String
showRatio r
  | denominator r == 1 = show $ numerator r
  | otherwise = show (numerator r) <> "/" <> show (denominator r)

prettyRatio :: RatioInt -> Doc ann
prettyRatio = pretty . showRatio

newtype SimpleSize = SimpleSize {getCoeffs :: Map SizeIdent RatioInt}

instance Semigroup SimpleSize where
  (<>) :: SimpleSize -> SimpleSize -> SimpleSize
  (<>) (SimpleSize c1) (SimpleSize c2) =
    SimpleSize $ M.filter (/= 0) (M.unionWith (+) c1 c2)

instance Monoid SimpleSize where
  {-# INLINE mempty #-}
  mempty :: SimpleSize
  mempty = SimpleSize mempty

instance Show SimpleSize where
  show :: SimpleSize -> String
  show (SimpleSize coef) =
    mconcat . cleanSum $ coefStr coef

instance Pretty SimpleSize where
  pretty :: SimpleSize -> Doc ann
  pretty = unsafeViaShow

instance VectorSpace SimpleSize where
  vScale :: RatioInt -> SimpleSize -> SimpleSize
  vScale c (SimpleSize coef)
    | c == 0 = mempty
    | otherwise = SimpleSize $ (* c) <$> coef

  {-# INLINE vSum #-}
  vSum :: SimpleSize -> SimpleSize -> SimpleSize
  vSum = (<>)

  {-# INLINE vIsNull #-}
  vIsNull :: SimpleSize -> Bool
  vIsNull (SimpleSize coef) = all (== 0) coef

-- | isCollinear a b: Return the k such that a = k * b
isCollinear :: SimpleSize -> SimpleSize -> Maybe RatioInt
isCollinear (SimpleSize a) (SimpleSize b) =
  case Map.keys $ Map.intersection a b of
    [] -> Nothing
    v : _ ->
      let vA = a Map.! v
          vB = b Map.! v
          k = vA / vB
          scaledB = (* k) <$> b
       in if vIsNull (vSub (SimpleSize a) (SimpleSize scaledB))
            then Just k
            else Nothing

-- | Size things
data Size = Size SimpleSize RatioInt

instance Semigroup Size where
  {-# INLINE (<>) #-}
  (<>) :: Size -> Size -> Size
  (<>) (Size c1 cst1) (Size c2 cst2) =
    Size (c1 <> c2) (cst1 + cst2)

instance Monoid Size where
  {-# INLINE mempty #-}
  mempty :: Size
  mempty = Size mempty 0

instance Show Size where
  show :: Size -> String
  show (Size (SimpleSize coef) cst) =
    mconcat . cleanSum $ coefStr coef ++ constantStr cst

instance Pretty Size where
  pretty :: Size -> Doc ann
  pretty = unsafeViaShow

instance VectorSpace Size where
  {-# INLINE vScale #-}
  vScale :: RatioInt -> Size -> Size
  vScale c (Size coef cst) = Size (vScale c coef) (c * cst)

  {-# INLINE vSum #-}
  vSum :: Size -> Size -> Size
  vSum = (<>)

  {-# INLINE vIsNull #-}
  vIsNull :: Size -> Bool
  vIsNull (Size coef cst) = cst == 0 && vIsNull coef

{-# INLINE constantSize #-}
constantSize :: RatioInt -> Size
constantSize = Size mempty

varSize :: SizeIdent -> Size
varSize sId = Size (SimpleSize $ M.singleton sId 1) 0

{-# INLINE scaleSize #-}
scaleSize :: RatioInt -> Size -> Size
scaleSize = vScale

{-# INLINE sumSize #-}
sumSize :: Size -> Size -> Size
sumSize = (<>)

{-# INLINE subSize #-}
subSize :: Size -> Size -> Size
subSize = vSub

sumVar :: SizeIdent -> SizeIdent -> Size
sumVar x y = sumSize (varSize x) (varSize y)

subVar :: SizeIdent -> SizeIdent -> Size
subVar x y = subSize (varSize x) (varSize y)

{-# INLINE isNull #-}
isNull :: Size -> Bool
isNull = vIsNull

subst :: (SizeIdent -> Size) -> Size -> Size
subst f (Size (SimpleSize coef) cst) =
  sumSize (constantSize cst) $
    M.foldMapWithKey (\v c -> scaleSize c $ f v) coef

hasVar :: Size -> SizeIdent -> Bool
hasVar (Size (SimpleSize coef) _) s = M.member s coef

{-# INLINE splitSize #-}
splitSize :: Size -> (SimpleSize, RatioInt)
splitSize (Size coefs cst) = (coefs, cst)

{-# INLINE toSize #-}
toSize :: SimpleSize -> Size
toSize x = Size x 0
