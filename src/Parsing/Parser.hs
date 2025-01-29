{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parsing.Parser (Parser, pFile) where

import Commons.Ast
import Commons.BiList
import Commons.Ids
import Commons.Position
import Commons.Tree
import Commons.Types
import Control.Monad (join, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Control.Monad.Combinators.NonEmpty as Comb (sepBy1, someTill)
import Control.Monad.Reader
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEmpty
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text.Lazy (Text, cons)
import Data.Void (Void)
import Parsing.Ast
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (Reader Int)

data Keyword
  = TRUE
  | FALSE
  | BOOL
  | INT
  | RAW
  | SIGNED
  | UNSIGNED
  | IF
  | THEN
  | ELSE
  | NOT
  | AND
  | OR
  | FBY
  | NODE
  | RETURNS
  | VAR
  | LET
  | TEL
  deriving (Eq, Ord, Enum, Bounded)

kwToString :: (IsString a) => Keyword -> a
kwToString TRUE = "true"
kwToString FALSE = "false"
kwToString BOOL = "bool"
kwToString INT = "int"
kwToString RAW = "raw"
kwToString SIGNED = "signed"
kwToString UNSIGNED = "unsigned"
kwToString IF = "if"
kwToString THEN = "then"
kwToString ELSE = "else"
kwToString NOT = "not"
kwToString AND = "and"
kwToString OR = "or"
kwToString FBY = "fby"
kwToString NODE = "node"
kwToString RETURNS = "returns"
kwToString VAR = "var"
kwToString LET = "let"
kwToString TEL = "tel"

isKeyword :: (Eq a, IsString a) => a -> Bool
isKeyword t = t `elem` (kwToString <$> [minBound .. maxBound])

merge :: Pos a -> Pos b -> c -> Pos c
merge (L beg _ _) (L _ _ end) c = L beg c end

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty (L.skipBlockComment "/*" "*/")

pLoc :: Parser a -> Parser (Pos a)
pLoc p = L <$> (spaceConsumer *> getOffset) <*> p <*> (getOffset <* spaceConsumer)

symbol :: Text -> Parser ()
symbol s = void $ spaceConsumer *> string s <* spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

comma :: Parser ()
comma = symbol ","

semicolon :: Parser ()
semicolon = symbol ";"

colon :: Parser ()
colon = symbol ":"

isAlphaNum :: Char -> Bool
isAlphaNum x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_'

pIdent :: Parser (Pos Ident)
pIdent = pLoc pIdent' <?> "identifier"
  where
    pIdent' = do
      o <- getOffset
      fstLetter <- satisfy isAsciiLower
      following <- takeWhileP Nothing isAlphaNum
      let t = cons fstLetter following
       in if isKeyword t
            then
              region (setErrorOffset o) $
                failure (Just . Label $ 'k' :| "eyword") (Set.singleton . Label $ 'i' :| "dentifier")
            else
              return (Ident t)

keyword :: Keyword -> Parser (Pos Text)
keyword k = pLoc pKeyword <?> "keyword"
  where
    pKeyword = try $ string (kwToString k) <* notFollowedBy (satisfy isAlphaNum)

pInteger :: Parser (Pos Integer)
pInteger = pLoc pInteger'
  where
    pInteger' = pBeginZero <|> L.decimal <?> "integer"
    pBeginZero = char '0' >> choice [pHexa, pOct, pBin, pDec, L.decimal, pZero]
    pHexa = char 'x' >> L.hexadecimal
    pOct = char 'o' >> L.octal
    pBin = char 'b' >> L.binary
    pDec = char 'd' >> L.decimal
    pZero = return 0

pBool :: Parser (Pos Bool)
pBool = pTrue <|> pFalse
  where
    pTrue = fmap (const True) <$> keyword TRUE
    pFalse = fmap (const False) <$> keyword FALSE

pType :: Parser (Pos LustreType)
pType = do
  defaultSize <- ask
  choice
    [ fmap (const BoolType) <$> keyword BOOL,
      pLoc pRaw,
      pLoc pUnsig,
      try (pLoc pSig),
      fmap (const (BitVectorType Signed (BVSize defaultSize))) <$> keyword INT
    ]
  where
    pRaw = char 'r' >> BitVectorType Raw . BVSize <$> L.decimal
    pUnsig = char 'u' >> BitVectorType Unsigned . BVSize <$> L.decimal
    pSig = char 'i' >> BitVectorType Signed . BVSize <$> L.decimal

pConstant :: Parser (Pos Constant)
pConstant =
  choice
    [ fmap IntegerConst <$> pInteger,
      fmap BoolConst <$> pBool
    ]

pPattern :: Parser Pattern
pPattern = pPatIdent <|> pPatTuple
  where
    pPatIdent = TreeLeaf <$> pIdent <?> "pattern"
    pPatTuple = buildPatTuple <$> pLoc (parens $ Comb.sepBy1 pPattern comma)
    buildPatTuple (L _ (e :| []) _) = e
    buildPatTuple (L _ (x :| (y : l)) _) = TreeNode (BiList x y l)

pConstantExpr :: Parser Expr
pConstantExpr = fmap ConstantExpr <$> pConstant

pIdentExpr :: Parser Expr
pIdentExpr = buildIdentExpr <$> pIdent
  where
    buildIdentExpr i@(L beg _ end) = L beg (IdentExpr i) end

pParensExprs :: Parser Expr
pParensExprs =
  buildExpr <$> pLoc (parens $ Comb.sepBy1 pExpr comma)
  where
    buildExpr (L _ (e :| []) _) = e
    buildExpr loc@(L _ (x :| (y : l)) _) = loc $> TupleExpr (BiList x y l)

pSliceExpr :: Parser Expr
pSliceExpr = pLoc pSliceExpr'
  where
    pSliceExpr' = try $ SliceExpr <$> pIdentExpr <*> brackets ((,) <$> L.decimal <* colon <*> L.decimal)

pSelectExpr :: Parser Expr
pSelectExpr = pLoc pSelectExpr'
  where
    pSelectExpr' = try (SelectExpr <$> pIdentExpr <*> brackets L.decimal)

pAppExpr :: Parser Expr
pAppExpr = pLoc pAppExpr'
  where
    pAppExpr' = try $ AppExpr <$> pIdent <*> parens (sepBy pExpr comma)

pIfExpr :: Parser Expr
pIfExpr = buildIf <$> getOffset <*> pCond <*> pTrueBranch <*> pFalseBranch
  where
    buildIf beg cond tb fb@(L _ _ end) = L beg (IfExpr cond tb fb) end
    pCond = keyword IF *> pExpr
    pTrueBranch = keyword THEN *> pExpr
    pFalseBranch = keyword ELSE *> pExpr

binary :: (Parser (Expr -> Expr -> Expr) -> a) -> Parser b -> (Expr -> Expr -> ExprDesc) -> a
binary cstr s f = cstr $ f' <$ s
  where
    f' a b = merge a b $ f a b

prefix :: Parser a -> (Expr -> ExprDesc) -> Operator Parser Expr
prefix s f = Prefix $ f' . L <$> getOffset <* s
  where
    f' cont expr@(L _ _ end) = cont (f expr) end

exprTable :: [[Operator Parser Expr]]
exprTable =
  [ [ prefix (symbol "+") unwrap,
      prefix (symbol "-") $ UnOpExpr UnNeg
    ],
    [ prefix (keyword NOT) $ UnOpExpr UnNot,
      prefix (keyword RAW) $ ConvertExpr Raw,
      prefix (keyword SIGNED) $ ConvertExpr Signed,
      prefix (keyword UNSIGNED) $ ConvertExpr Unsigned
    ],
    [binary InfixL (symbol "++") $ ConcatExpr],
    [ binary InfixL (symbol "+") $ BinOpExpr BinAdd,
      binary InfixL (symbol "-") $ BinOpExpr BinSub
    ],
    [ binary InfixN (symbol "==") $ BinOpExpr BinEq,
      binary InfixN (symbol "<>") $ BinOpExpr BinNeq,
      binary InfixN (symbol ">=") $ BinOpExpr BinGe,
      binary InfixN (symbol "<=") $ BinOpExpr BinLe,
      binary InfixN (symbol ">") $ BinOpExpr BinGt,
      binary InfixN (symbol "<") $ BinOpExpr BinLt
    ],
    [binary InfixR (keyword AND) $ BinOpExpr BinAnd],
    [binary InfixR (keyword OR) $ BinOpExpr BinOr],
    [binary InfixR (keyword FBY) FbyExpr]
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm exprTable <?> "expression"

pTerm :: Parser Expr
pTerm =
  choice
    [ pIfExpr,
      pSliceExpr,
      pSelectExpr,
      pAppExpr,
      pConstantExpr,
      pIdentExpr,
      pParensExprs
    ]

pEquation :: Parser Equation
pEquation = Equation <$> (pPattern <* symbol "=") <*> pExpr <* semicolon

pDecl :: Parser (NonEmpty IdentDecl)
pDecl = buildDecls <$> Comb.sepBy1 pIdent comma <*> (colon *> pType)
  where
    buildDecls l t = flip IdentDecl t <$> l

pNode :: Parser PNode
pNode =
  keyword NODE
    >> PNode
      <$> pIdent
      <*> (pInputDecl <* keyword RETURNS)
      <*> (pOutputDecl <* semicolon)
      <*> pLocals
      <*> Comb.someTill pEquation (keyword TEL)
  where
    pInputDecl = concat <$> parens (sepBy pDeclList semicolon)
    pOutputDecl = join <$> parens (Comb.sepBy1 pDecl semicolon)
    pLocals = (concat <$> pLocalsList) <|> keyword LET $> []
    pLocalsList = keyword VAR *> someTill (pDeclList <* semicolon) (keyword LET)
    pDeclList = NEmpty.toList <$> pDecl

pFile :: Parser PAst
pFile = PAst <$> many pNode <* eof
