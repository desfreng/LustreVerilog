{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parsing.Parser
  ( Parser,
    Localized (..),
    pFile,
    pNode,
    pDecl,
    pEquation,
    pExpr,
    pPattern,
    pType,
    pConstant,
    spaceConsumer,
    module Parsing.Ast,
  )
where

import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.NonEmpty as Comb (sepBy1)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEmpty
import Data.Text.Lazy (Text, cons)
import Data.Void (Void)
import Parsing.Ast
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

merge :: Localized a -> Localized b -> c -> Localized c
merge (L beg _ _) (L _ _ end) c = L beg c end

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty (L.skipBlockComment "/*" "*/")

pLocalized :: Parser a -> Parser (Localized a)
pLocalized p = L <$> (spaceConsumer *> getOffset) <*> p <*> (getOffset <* spaceConsumer)

symbol :: Text -> Parser ()
symbol s = void $ spaceConsumer *> string s <* spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

comma :: Parser ()
comma = symbol ","

semicolon :: Parser ()
semicolon = symbol ";"

colon :: Parser ()
colon = symbol ":"

isAlphaNum :: Char -> Bool
isAlphaNum x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_'

pIdent :: Parser (Localized Ident)
pIdent = pLocalized (Ident <$> pIdent') <?> "identifier"
  where
    pIdent' = cons <$> satisfy isAsciiLower <*> takeWhileP Nothing isAlphaNum

keyword :: Text -> Parser (Localized Text)
keyword s = pLocalized pKeyword <?> "keyword"
  where
    pKeyword = try $ string s <* notFollowedBy (satisfy isAlphaNum)

pInteger :: Parser (Localized Integer)
pInteger = pLocalized pInteger'
  where
    pInteger' = pBeginZero <|> L.decimal <?> "integer"
    pBeginZero = char '0' >> choice [pHexa, pOct, pBin, pDec, L.decimal, pZero]
    pHexa = char 'x' >> L.hexadecimal
    pOct = char 'o' >> L.octal
    pBin = char 'b' >> L.binary
    pDec = char 'd' >> L.decimal
    pZero = return 0

pBool :: Parser (Localized Bool)
pBool = pTrue <|> pFalse
  where
    pTrue = fmap (const True) <$> keyword "true"
    pFalse = fmap (const False) <$> keyword "false"

pType :: Parser (Localized AtomicType)
pType =
  choice
    [ fmap (const BoolType) <$> keyword "bool",
      fmap (const IntegerType) <$> keyword "int"
    ]

pConstant :: Parser (Localized Constant)
pConstant =
  choice
    [ fmap IntegerConst <$> pInteger,
      fmap BoolConst <$> pBool
    ]

pPattern :: Parser Pattern
pPattern = pPatIdent <|> pPatTuple
  where
    pPatIdent = PatIdent <$> pIdent <?> "pattern"
    pPatTuple = buildPatTuple <$> pLocalized (parens $ Comb.sepBy1 pPattern comma)
    buildPatTuple (L _ (e :| []) _) = e
    buildPatTuple (L _ (x :| (y : l)) _) = PatTuple (BiList x y l)

pConstantExpr :: Parser Expr
pConstantExpr = fmap ConstantExpr <$> pConstant

pIdentExpr :: Parser Expr
pIdentExpr = buildIdentExpr <$> pIdent
  where
    buildIdentExpr i@(L beg _ end) = L beg (IdentExpr i) end

pParensExprs :: Parser Expr
pParensExprs =
  buildExpr <$> pLocalized (parens $ Comb.sepBy1 pExpr comma)
  where
    buildExpr (L _ (e :| []) _) = e
    buildExpr loc@(L _ (x :| (y : l)) _) = loc $> TupleExpr (BiList x y l)

pAppExpr :: Parser Expr
pAppExpr = pLocalized pAppExpr'
  where
    pAppExpr' = try $ AppExpr <$> pIdent <*> parens (sepBy pExpr comma)

pIfExpr :: Parser Expr
pIfExpr = buildIf <$> getOffset <*> pCond <*> pTrueBranch <*> pFalseBranch
  where
    buildIf beg cond tb fb@(L _ _ end) = L beg (IfExpr cond tb fb) end
    pCond = keyword "if" *> pExpr
    pTrueBranch = keyword "then" *> pExpr
    pFalseBranch = keyword "else" *> pExpr

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
      prefix (symbol "-") $ UnOpExpr UnMinus
    ],
    [prefix (keyword "not") $ UnOpExpr UnNot],
    [ binary InfixL (symbol "+") $ BinOpExpr BinAdd,
      binary InfixL (symbol "-") $ BinOpExpr BinSub
    ],
    [ binary InfixN (symbol "==") $ BinOpExpr BinEq,
      binary InfixN (symbol "!=") $ BinOpExpr BinNeq,
      binary InfixN (symbol "<>") $ BinOpExpr BinNeq,
      binary InfixN (symbol ">=") $ BinOpExpr BinGe,
      binary InfixN (symbol "<=") $ BinOpExpr BinLe,
      binary InfixN (symbol ">") $ BinOpExpr BinGt,
      binary InfixN (symbol "=") $ BinOpExpr BinEq,
      binary InfixN (symbol "<") $ BinOpExpr BinLt
    ],
    [binary InfixR (keyword "and") $ BinOpExpr BinAnd],
    [binary InfixR (keyword "or") $ BinOpExpr BinOr],
    [binary InfixR (keyword "fby") FbyExpr]
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm exprTable <?> "expression"

pTerm :: Parser Expr
pTerm =
  choice
    [ pIfExpr,
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

pNode :: Parser (Localized Node)
pNode =
  pNode'
    <$> keyword "node"
    <*> pIdent
    <*> (pInputDecl <* keyword "returns")
    <*> (pOutputDecl <* semicolon)
    <*> pLocals
    <*> some (try pEquation)
    <*> keyword "tel"
  where
    pInputDecl = concat <$> parens (sepBy pDeclList semicolon)
    pOutputDecl = join <$> parens (Comb.sepBy1 pDecl semicolon)
    pLocals = keyword "var" *> pLocalsList <* keyword "let" <|> keyword "let" $> []
    pLocalsList = fmap concat $ some . try $ pDeclList <* semicolon
    pDeclList = NEmpty.toList <$> pDecl
    pNode' beg nodeName nodeInputs nodeOutputs nodeLocals nodeEqs end = merge beg end $ Node {..}

pFile :: Parser Ast
pFile = Ast <$> many pNode <* eof