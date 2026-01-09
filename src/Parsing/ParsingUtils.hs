module Parsing.ParsingUtils where

import Commons.Ast (Bound (..), Constant (..), Interval (..), SizeConstraint)
import Commons.BiList (BiList (..))
import Commons.Ids (Ident, SizeIdent)
import Commons.Position (Pos, extendLeft, extendRight, fromLoc, merge, unwrap)
import Commons.Tree (Tree (..))
import Commons.Types (BitVectorKind (Signed))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Parsing.Ast
import Parsing.Commons (Token (..))
import Parsing.ParsingMonad (ParsingMonad, defaultIntSize, readParam)
import Parsing.RevList (RevList, flatten, toNonEmpty)

-- * Location Utils

{-# INLINE loc #-}
loc :: Token -> a -> Pos a
loc (T start _ end) x = fromLoc start x end

{-# INLINE locBetween #-}
locBetween :: Token -> a -> Token -> Pos a
locBetween (T start _ _) x (T _ _ end) = fromLoc start x end

-- * Size Eq Utils

mkLMulSize :: Token -> SizeExpr -> ParsingMonad Int -> ParsingMonad SizeExpr
mkLMulSize (T start _ _) e iVal = do
  i <- iVal
  return $ extendLeft (`MulSize` i) start e

mkRMulSize :: SizeExpr -> Token -> ParsingMonad Int -> ParsingMonad SizeExpr
mkRMulSize e (T _ _ end) iVal = do
  i <- iVal
  return $ extendRight (`MulSize` i) e end

mkDivSize :: SizeExpr -> Token -> ParsingMonad Int -> ParsingMonad SizeExpr
mkDivSize e (T _ _ end) iVal = do
  i <- iVal
  return $ extendRight (`DivSize` i) e end

mkSizeInt :: Token -> ParsingMonad Int -> ParsingMonad SizeExpr
mkSizeInt tok i = loc tok . ConstantSize <$> i

-- * Type and Declaration

{-# INLINE bvType #-}
bvType :: Token -> BitVectorKind -> SizeExpr -> Token -> Pos LustreType
bvType start k s = locBetween start (BitVectorType k s)

mkIntType :: Token -> ParsingMonad (Pos LustreType)
mkIntType t =
  loc t . BitVectorType Signed . loc t . ConstantSize <$> readParam defaultIntSize

{-# INLINE decl #-}
decl :: RevList (Pos Ident) -> Pos LustreType -> NonEmpty IdentDecl
decl l t = flip IdentDecl t <$> toNonEmpty l

-- * Patterns

patTuple :: RevList Pattern -> Pattern
patTuple r = case toNonEmpty r of
  x :| [] -> x
  x :| y : tl -> TreeNode $ BiList x y tl

-- * Expressions

{-# INLINE mkUnary #-}
mkUnary :: Token -> (Pos a -> a) -> Pos a -> Pos a
mkUnary (T beg _ _) f = extendLeft f beg

mkIf :: Token -> Expr -> Expr -> Expr -> Expr
mkIf (T beg _ _) cond tBranch fBranch =
  let e = merge IfExpr cond tBranch
      e' = merge unwrap e fBranch
   in extendLeft unwrap beg e'

{-# INLINE mkBool #-}
mkBool :: Token -> Bool -> Expr
mkBool tok v = loc tok $ ConstantExpr $ BoolConst v

{-# INLINE mkInt #-}
mkInt :: Token -> Integer -> Expr
mkInt tok v = loc tok $ ConstantExpr $ IntegerConst v

mkExprTuple :: Token -> RevList Expr -> Token -> Expr
mkExprTuple beg l end = case toNonEmpty l of
  x :| [] -> x
  x :| y : tl -> locBetween beg (TupleExpr $ BiList x y tl) end

{-# INLINE mkSelect #-}
mkSelect :: Expr -> SizeExpr -> Token -> Expr
mkSelect e s (T _ _ end) = extendRight (`SelectExpr` s) e end

{-# INLINE mkSlice #-}
mkSlice :: Expr -> SizeExpr -> SizeExpr -> Token -> Expr
mkSlice e x y (T _ _ end) = extendRight (`SliceExpr` (x, y)) e end

{-# INLINE mkApp #-}
mkApp :: Pos Ident -> [Expr] -> Token -> Expr
mkApp fName l (T _ _ end) = extendRight (`AppExpr` l) fName end

-- * Node and Size constructor

data CritOp = Lt | Leq | Eq | Geq | Gt

mkBinCrit :: SizeExpr -> CritOp -> SizeExpr -> Pos (Interval, SizeExpr)
mkBinCrit beg op end =
  let int l r =
        let v = merge SubSize r l
            i = case op of
              Lt -> MinoredBy (Ex 0)
              Leq -> MinoredBy (In 0)
              Eq -> Between (In 0) (In 0)
              Geq -> MajoredBy (In 0)
              Gt -> MajoredBy (Ex 0)
         in (i, v)
   in merge int beg end

{-# INLINE mkNode #-}
mkNode :: Pos Ident -> [IdentDecl] -> NonEmpty IdentDecl -> ([Pos SizeIdent], [SizeConstraint SizeExpr]) -> PBody -> PNode
mkNode name inputs outputs (sVars, sizeConstr) =
  PNode name inputs outputs sVars sizeConstr

mkBody :: Maybe (RevList (NonEmpty IdentDecl)) -> RevList Equation -> PNodeBody
mkBody Nothing eqs = PNodeBody [] $ toNonEmpty eqs
mkBody (Just locals) eqs = PNodeBody (toList $ flatten locals) $ toNonEmpty eqs
