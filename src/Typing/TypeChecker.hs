{-# LANGUAGE TupleSections #-}

module Typing.TypeChecker (typeAst, TypeErrorDesc, module Typing.Ast) where

import Control.Applicative
import Data.Bifunctor
import Data.Foldable ()
import Data.Functor
import Data.Text.Lazy (Text)
import Parsing.Ast
import Text.Megaparsec.Error (ParseErrorBundle)
import Typing.Ast
import Typing.Environments
import Typing.TypeError

typeAst :: FilePath -> Text -> Ast -> Either (ParseErrorBundle Text TypeErrorDesc) TAst
typeAst path txt (Ast l) = runCanFail path txt $ runNodeEnv $ mapM_ typeNode l

typeNode :: Localized Node -> NodeEnv ()
typeNode locNode = do
  (nodeCtx, tEqs) <- evalIn typeVarDecls (canFailMapM typeEq (nodeEqs node))
  addNode locNode (nodeName node) nodeCtx tEqs
  where
    node = unwrap locNode

    typeVarDecls :: NodeVarEnv ()
    typeVarDecls = do
      mapM_ (typeDecl addInputVariable) (nodeInputs node)
      mapM_ (typeDecl addOutputVariable) (nodeOutputs node)
      mapM_ (typeDecl addLocalVariable) (nodeLocals node)

typeDecl :: (Localized Ident -> CanFail AtomicType -> NodeVarEnv ()) -> IdentDecl -> NodeVarEnv ()
typeDecl f (IdentDecl declIdent declType) = typeType declType >>= f declIdent

typeType :: Localized AtomicType -> NodeVarEnv (CanFail AtomicType)
typeType = return . pure . unwrap

typeEq :: Equation -> ExprEnv (CanFail TEquation)
typeEq (Equation p e) = typePat p >>= fmap collapse . traverse buildEq
  where
    buildEq :: (TPattern, TType) -> ExprEnv (CanFail TEquation)
    buildEq (tp, typ) = fmap (tp,) <$> checkExprType typ e

typePat :: Pattern -> ExprEnv (CanFail (TPattern, TType))
typePat (PatIdent x) = fmap (bimap TPatAtomic Atom) <$> findVariable x
typePat (PatTuple l) = fmap (bimap TPatTuple Tuple . Typing.Ast.unzip) . sequenceA <$> mapM typePat l

checkExprType :: TType -> Expr -> ExprEnv (CanFail TExpr)
checkExprType typ e = checkExprType' (unwrap e)
  where
    checkExprType' (ConstantExpr c) = collapse . fmap assertTyp <$> typeConstantExpr c
    checkExprType' (IdentExpr i) = collapse . fmap assertTyp <$> typeIdentExpr i
    checkExprType' (UnOpExpr op arg) = collapse . fmap assertTyp <$> typeUnOpExpr op arg
    checkExprType' (BinOpExpr op lhs rhs) = collapse . fmap assertTyp <$> typeBinOpExpr e op lhs rhs
    checkExprType' (AppExpr n args) = collapse . fmap assertTyp <$> typeAppExpr e n args
    checkExprType' (TupleExpr l) = fmap (e $>) <$> checkTupleExprType e typ l
    checkExprType' (IfExpr cond tb fb) = fmap (e $>) <$> checkIfExprType typ cond tb fb
    checkExprType' (FbyExpr arg arg') = fmap (e $>) <$> checkFbyExprType typ arg arg'

    assertTyp (te, ty) =
      let typeErr expd fnd = reportError e $ InvalidType expd fnd typ ty
       in compareType typeErr typ ty $> (e $> (te, ty))

compareType :: (TType -> TType -> CanFail ()) -> TType -> TType -> CanFail ()
compareType f t1 t2 = case (t1, t2) of
  (Atom x, Atom y) -> if x == y then pure () else f t1 t2
  (Tuple t1L, Tuple t2L) ->
    if length t1L == length t2L
      then foldl compareElm (pure ()) $ Typing.Ast.zip t1L t2L
      else f t1 t2
  (Atom _, Tuple _) -> f t1 t2
  (Tuple _, Atom _) -> f t1 t2
  where
    compareElm acc (t1E, t2E) = (\() () -> ()) <$> acc <*> compareType f t1E t2E

findExprType :: Expr -> ExprEnv (CanFail TExpr)
findExprType e = fmap (e $>) <$> findExprType' (unwrap e)
  where
    findExprType' (ConstantExpr c) = typeConstantExpr c
    findExprType' (IdentExpr i) = typeIdentExpr i
    findExprType' (UnOpExpr op arg) = typeUnOpExpr op arg
    findExprType' (BinOpExpr op lhs rhs) = typeBinOpExpr e op lhs rhs
    findExprType' (AppExpr n args) = typeAppExpr e n args
    findExprType' (TupleExpr l) = findTupleExprType l
    findExprType' (IfExpr cond tb fb) = findIfExprExpr e cond tb fb
    findExprType' (FbyExpr arg arg') = findFbyExprExpr e arg arg'

typeConstantExpr :: Constant -> ExprEnv (CanFail (TExprDesc, TType))
typeConstantExpr c@(BoolConst _) = return . pure $ (ConstantTExpr c, Atom BoolType)
typeConstantExpr c@(IntegerConst _) = return . pure $ (ConstantTExpr c, Atom IntegerType)

typeIdentExpr :: Localized Ident -> ExprEnv (CanFail (TExprDesc, TType))
typeIdentExpr v = fmap (bimap VarTExpr Atom) <$> findVariable v

typeUnOpExpr :: UnOp -> Expr -> ExprEnv (CanFail (TExprDesc, TType))
typeUnOpExpr UnNot e =
  fmap ((,Atom BoolType) . UnOpTExpr UnMinus)
    <$> checkExprType (Atom BoolType) e
typeUnOpExpr UnMinus e =
  fmap ((,Atom IntegerType) . UnOpTExpr UnMinus)
    <$> checkExprType (Atom IntegerType) e

typeBinOpExpr :: Localized a -> BinOp -> Expr -> Expr -> ExprEnv (CanFail (TExprDesc, TType))
typeBinOpExpr l op lhs rhs =
  case op of
    BinEq -> cmpOp
    BinNeq -> cmpOp
    BinLt -> cmpOp
    BinLe -> cmpOp
    BinGt -> cmpOp
    BinGe -> cmpOp
    BinAdd -> intOp
    BinSub -> intOp
    BinAnd -> boolOp
    BinOr -> boolOp
  where
    cmpOp = collapse <$> (liftA2 buildCmpOp <$> findExprType lhs <*> findExprType rhs)
    buildCmpOp tLhs tRhs =
      let lhsTyp = exprType tLhs
          rhsTyp = exprType tRhs
          typeErr t1 t2 = reportError l $ NotSameType t1 t2 lhsTyp rhsTyp
       in compareType typeErr lhsTyp rhsTyp $> (BinOpTExpr op tLhs tRhs, Atom BoolType)

    intOp = liftA2 buildIntOp <$> checkExprType (Atom IntegerType) lhs <*> checkExprType (Atom IntegerType) rhs
    buildIntOp tLhs tRhs = (BinOpTExpr op tLhs tRhs, Atom IntegerType)

    boolOp = liftA2 buildBoolOp <$> checkExprType (Atom BoolType) lhs <*> checkExprType (Atom BoolType) rhs
    buildBoolOp tLhs tRhs = (BinOpTExpr op tLhs tRhs, Atom BoolType)

typeAppExpr :: Localized a -> Localized Ident -> [Expr] -> ExprEnv (CanFail (TExprDesc, TType))
typeAppExpr loc node args = findNode node >>= fmap collapse . traverse typeAppExpr'
  where
    typeAppExpr' :: (Localized NodeIdent, NodeSignature) -> ExprEnv (CanFail (TExprDesc, TType))
    typeAppExpr' (nodeId, nodeSign) =
      fmap ((,outputType nodeSign) . AppTExpr nodeId) <$> checkInputArgs nodeSign

    checkInputArgs :: NodeSignature -> ExprEnv (CanFail [TExpr])
    checkInputArgs nodeSign =
      let nodeArr = nodeArity nodeSign
          nbArgs = length args
       in case compare nodeArr nbArgs of
            LT -> return . reportError loc $ TooManyArguments nodeArr nbArgs
            EQ -> sequenceA <$> mapM checkInput (Prelude.zip (inputTypes nodeSign) args)
            GT -> return . reportError loc $ MissingArgument nodeArr nbArgs

    checkInput (atyp, e) = checkExprType (Atom atyp) e

findTupleExprType :: BiList Expr -> ExprEnv (CanFail (TExprDesc, TType))
findTupleExprType l = fmap buildTupleExpr . sequenceA <$> mapM findExprType l
  where
    buildTupleExpr exprL =
      (TupleTExpr exprL, Tuple $ exprType <$> exprL)

findIfExprExpr :: Localized a -> Expr -> Expr -> Expr -> ExprEnv (CanFail (TExprDesc, TType))
findIfExprExpr loc cond tb fb =
  collapse
    <$> ( liftA3
            (buildIfExpr loc)
            <$> checkExprType (Atom BoolType) cond
            <*> findExprType tb
            <*> findExprType fb
        )
  where
    buildIfExpr l tCond tTB tFB =
      let tbTyp = exprType tTB
          fbTyp = exprType tFB
          typeErr t1 t2 = reportError l $ NotSameType t1 t2 tbTyp fbTyp
       in compareType typeErr tbTyp fbTyp $> (IfTExpr tCond tTB tFB, tbTyp)

findFbyExprExpr :: Localized a -> Expr -> Expr -> ExprEnv (CanFail (TExprDesc, TType))
findFbyExprExpr loc initE nextE =
  collapse
    <$> ( liftA2 (buildFbyExpr loc)
            <$> findExprType initE
            <*> findExprType nextE
        )
  where
    buildFbyExpr l tInitE tNextE =
      let initTyp = exprType tInitE
          nextTyp = exprType tNextE
          typeErr t1 t2 = reportError l $ NotSameType t1 t2 initTyp nextTyp
       in compareType typeErr initTyp nextTyp $> (FbyTExpr tInitE tNextE, initTyp)

checkTupleExprType :: Localized a -> TType -> BiList Expr -> ExprEnv (CanFail (TExprDesc, TType))
checkTupleExprType loc typ@(Atom _) _ = return . reportError loc $ UnExpectedTuple typ
checkTupleExprType loc typ@(Tuple typL) l =
  let typArr = length typL
      nbArgs = length l
   in case compare typArr nbArgs of
        LT -> return . reportError loc $ TooManyExpressions typArr nbArgs
        EQ -> fmap buildTuple . sequenceA <$> mapM (uncurry checkExprType) (Typing.Ast.zip typL l)
        GT -> return . reportError loc $ MissingExpression typArr nbArgs
  where
    buildTuple tl = (TupleTExpr tl, typ)

checkIfExprType :: TType -> Expr -> Expr -> Expr -> ExprEnv (CanFail (TExprDesc, TType))
checkIfExprType typ cond tb fb = do
  tCond <- checkExprType (Atom BoolType) cond
  tTB <- checkExprType typ tb
  tFB <- checkExprType typ fb
  return $ buildIfExpr <$> tCond <*> tTB <*> tFB
  where
    buildIfExpr tCond tTB tFB = (IfTExpr tCond tTB tFB, typ)

checkFbyExprType :: TType -> Expr -> Expr -> ExprEnv (CanFail (TExprDesc, TType))
checkFbyExprType typ initE nextE = do
  tInitE <- checkExprType typ initE
  tNextE <- checkExprType typ nextE
  return $ buildFbyExpr <$> tInitE <*> tNextE
  where
    buildFbyExpr tInitE tNextE = (FbyTExpr tInitE tNextE, typ)
