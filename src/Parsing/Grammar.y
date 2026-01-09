{
module Parsing.Grammar where

import Commons.Ast
import Commons.Ids (Ident, SizeIdent)
import Commons.Position
import Commons.Tree
import Commons.Types
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Parsing.Ast
import Parsing.Commons
import Parsing.ParsingMonad
import Parsing.ParsingUtils
import Parsing.RevList (RevList)
import qualified Parsing.RevList as R
import Data.Bits (toIntegralSized)
import Text.Printf (printf)
import Prelude hiding (toInteger)

}

%name parser File
%tokentype { Token }
%error { parseError }
%monad { ParsingMonad } { >>= } { return }
%lexer { lexer } { T _ TokEOF _ }
%expect 0

%token
  true          { T _ TokTrue _ }
  false         { T _ TokFalse _ }
  bool          { T _ TokBool _ }
  int           { T _ TokIntKw _ }
  raw           { T _ TokRaw _ }
  signed        { T _ TokSigned _ }
  unsigned      { T _ TokUnsigned _ }
  r             { T _ TokR _ }
  i             { T _ TokI _ }
  u             { T _ TokU _ }
  if            { T _ TokIf _ }
  then          { T _ TokThen _ }
  else          { T _ TokElse _ }
  not           { T _ TokNot _ }
  and           { T _ TokAnd _ }
  or            { T _ TokOr _ }
  fby           { T _ TokFby _ }
  node          { T _ TokNode _ }
  returns       { T _ TokReturns _ }
  size          { T _ TokSize _ }
  where         { T _ TokWhere _ }
  var           { T _ TokVar _ }
  let           { T _ TokLet _ }
  tel           { T _ TokTel _ }
  when          { T _ TokWhen _ }
  otherwise     { T _ TokOtherwise _ }

  '('           { T _ TokLParen _ }
  ')'           { T _ TokRParen _ }
  '['           { T _ TokLBrack _ }
  ']'           { T _ TokRBrack _ }
  ','           { T _ TokComma _ }
  ';'           { T _ TokSemi _ }
  ':'           { T _ TokColon _ }
  '='           { T _ TokEq _ }

  '=='          { T _ TokEqEq _ }
  '<>'          { T _ TokNeq _ }
  '>='          { T _ TokGe _ }
  '<='          { T _ TokLe _ }
  '>'           { T _ TokGt _ }
  '<'           { T _ TokLt _ }
  '+'           { T _ TokPlus _ }
  '-'           { T _ TokMinus _ }
  '*'           { T _ TokAst _ }
  '/'           { T _ TokSlash _ }
  '++'          { T _ TokPlusPlus _ }

  Ident_        { T _ (TokIdent _) _ }
  SizeIdent_    { T _ (TokSizeIdent _) _ }
  Integer       { T _ (TokInteger _) _ }

%nonassoc SIZE_INT
%nonassoc else
%right fby
%left or
%left and
%nonassoc '==' '<>' '>=' '<=' '>' '<'
%left '++'
%left '+' '-'
%left '*' '/'
%nonassoc NEG not raw signed unsigned
%nonassoc '['

%%

CommaList(p) :: { RevList p }
    : p                                     {  R.singleton $1 }
    | CommaList(p) ',' p                    {  R.snoc $1 $3 }
    
SemiList(p) :: { RevList p }
    : p                                     {  R.singleton $1 }
    | SemiList(p) ';' p                     {  R.snoc $1 $3 }

List(p) :: { RevList p }
    : p                                     {  R.singleton $1 }
    | List(p) p                             {  R.snoc $1 $2 }

Ident :: { Pos Ident }
    : Ident_                                {  toIdent $1 }

SizeIdent :: { Pos SizeIdent }
    : SizeIdent_                            {  toSizeIdent $1 }

Maybe(p) :: { Maybe p }
    :                                       {  Nothing }
    | p                                     {  Just $1 }

-- Entry Point
File :: { PAst }
    : List(Node)                            {  PAst $ R.toList $1 }

Node :: { PNode }
    : node Ident
      '(' Inputs ')' Maybe(';')
      returns
      '(' Outputs ')' Maybe(';')
      SizeDecl
      BodyDecls                             {  mkNode $2 $4 $9 $12 $13 }

Inputs :: { [IdentDecl] }
    :                                       {  [] }
    |  SemiList(Decl)                       {  NE.toList $ R.flatten $1 }

Outputs :: { NonEmpty IdentDecl }
    :  SemiList(Decl)                       {  R.flatten $1 }

SizeDecl :: { ([Pos SizeIdent], [SizeConstraint SizeExpr]) }
    :                                       {  ([], [])  }
    | size CommaList(SizeIdent) Maybe(';')
      SizeContext                           {  (R.toList $2, $4) }             

SizeContext :: { [SizeConstraint SizeExpr] }
    :                                       {  [] }
    | where SemiList(SizeConstraint)
      Maybe(';')                            {  R.toList $2 }

SizeConstraint :: { SizeConstraint SizeExpr }
    : SizeExpr '==' SizeExpr                {  EqConstr $1 $3 }
    | SizeExpr '>=' SizeExpr                {  GeqConstr $1 $3 }
    | SizeExpr '<=' SizeExpr                {  LeqConstr $1 $3 }
    | SizeExpr '<' SizeExpr                 {  LtConstr $1 $3 }
    | SizeExpr '>' SizeExpr                 {  GtConstr $1 $3 }

Decl :: { NonEmpty IdentDecl }
    : CommaList(Ident) ':' Type             {  decl $1 $3 }

BodyDecls :: { PBody }
    : NodeBody                              {  PSimpleBody $1 }
    | when Criterion NodeBody
      otherwise NodeBody                    {  PComposedBody $2 $3 (loc $4 ()) $5 }

Criterion :: { Pos (Interval, SizeExpr) }
    : TernCriterion                         {  $1 }
    | BinCriterion                          {  $1 }

TernCriterion :: { Pos (Interval, SizeExpr) }
    : Integer '<' SizeExpr '<' Integer      {% mkTern $1 Ex (toInt $1) $3 Ex (toInt $5) $5 }
    | Integer '<' SizeExpr '<=' Integer     {% mkTern $1 Ex (toInt $1) $3 In (toInt $5) $5 }
    | Integer '<=' SizeExpr '<' Integer     {% mkTern $1 In (toInt $1) $3 Ex (toInt $5) $5 }
    | Integer '<=' SizeExpr '<=' Integer    {% mkTern $1 In (toInt $1) $3 In (toInt $5) $5 }
    | Integer '>' SizeExpr '>' Integer      {% mkTern $1 Ex (toInt $5) $3 Ex (toInt $1) $5 }
    | Integer '>' SizeExpr '>=' Integer     {% mkTern $1 In (toInt $5) $3 Ex (toInt $1) $5 }
    | Integer '>=' SizeExpr '>' Integer     {% mkTern $1 Ex (toInt $5) $3 In (toInt $1) $5 }
    | Integer '>=' SizeExpr '>=' Integer    {% mkTern $1 In (toInt $5) $3 In (toInt $1) $5 }

BinCriterion :: { Pos (Interval, SizeExpr) }
    : SizeExpr '<' SizeExpr                 {  mkBinCrit $1 Lt $3 }
    | SizeExpr '<=' SizeExpr                {  mkBinCrit $1 Leq $3 }
    | SizeExpr '==' SizeExpr                {  mkBinCrit $1 Eq $3 }
    | SizeExpr '>=' SizeExpr                {  mkBinCrit $1 Geq $3 }
    | SizeExpr '>' SizeExpr                 {  mkBinCrit $1 Gt $3 }

NodeBody :: { PNodeBody }
    : let List(Equation) tel                {  mkBody Nothing $2 }
    | var SemiList(Decl)
      Maybe(';')
      let List(Equation) tel                {  mkBody (Just $2) $5 }

Equation :: { Equation }
    : Pattern '=' Expr ';'                  {  Equation $1 $3 }

Pattern :: { Pattern }
    : Ident                                 {  TreeLeaf $1 }
    | '(' CommaList(Pattern) ')'            {  patTuple $2 }

Expr :: { Expr }
    : SimpleExpr                            {  $1 }
    | '+' Expr                              {  $2 }
    | '-' Expr          %prec NEG           {  mkUnary $1 (UnOpExpr UnNeg) $2 }
    | not Expr                              {  mkUnary $1 (UnOpExpr UnNot) $2 }
    | raw Expr                              {  mkUnary $1 (ConvertExpr Raw) $2 }
    | signed Expr                           {  mkUnary $1 (ConvertExpr Signed) $2 }
    | unsigned Expr                         {  mkUnary $1 (ConvertExpr Unsigned) $2 }
    | Expr '++' Expr                        {  merge ConcatExpr $1 $3 }
    | Expr '+' Expr                         {  merge (BinOpExpr BinAdd) $1 $3 }
    | Expr '-' Expr                         {  merge (BinOpExpr BinSub) $1 $3 }
    | Expr '==' Expr                        {  merge (BinOpExpr BinEq) $1 $3 }
    | Expr '>=' Expr                        {  merge (BinOpExpr BinGe) $1 $3 }
    | Expr '<=' Expr                        {  merge (BinOpExpr BinLe) $1 $3 }
    | Expr '<>' Expr                        {  merge (BinOpExpr BinNeq) $1 $3 }
    | Expr '>' Expr                         {  merge (BinOpExpr BinGt) $1 $3 }
    | Expr '<' Expr                         {  merge (BinOpExpr BinLt) $1 $3 }
    | Expr and Expr                         {  merge (BinOpExpr BinAnd) $1 $3 }
    | Expr or Expr                          {  merge (BinOpExpr BinOr) $1 $3 }
    | Expr fby Expr                         {  merge FbyExpr $1 $3 }
    | if Expr then Expr else Expr           {  mkIf $1 $2 $4 $6 }

SimpleExpr :: { Expr }
    : Ident                                 {  fmap IdentExpr $1 }
    | Integer                               {  mkInt $1 $ toInteger $1 }
    | true                                  {  mkBool $1 True }
    | false                                 {  mkBool $1 False }
    | '(' CommaList(Expr) ')'               {  mkExprTuple $1 $2 $3 }
    | Expr '[' SizeExpr ']'                 {  mkSelect $1 $3 $4 }
    | Expr '[' SizeExpr ':' SizeExpr ']'    {  mkSlice $1 $3 $5 $6 }
    | Ident '(' ArgsList ')'                {  mkApp $1 $3 $4 }

ArgsList :: { [Expr] }
    :                                       {  [] }
    | CommaList(Expr)                       {  R.toList $1 }

Type :: { Pos LustreType }
    : bool                                  {  loc $1 BoolType }
    | int                                   {% mkIntType $1 }
    | r '<' SizeExpr '>'                    {  bvType $1 Raw $3 $4 }
    | u '<' SizeExpr '>'                    {  bvType $1 Unsigned $3 $4 }
    | i '<' SizeExpr '>'                    {  bvType $1 Signed $3 $4 }

SizeExpr :: { SizeExpr }
    : SizeExpr '+' SizeExpr                 {  merge AddSize $1 $3 }
    | SizeExpr '-' SizeExpr                 {  merge SubSize $1 $3 }
    | Integer '*' SizeExpr                  {% mkLMulSize $1 $3 $ toInt $1 }
    | SizeExpr '*' Integer                  {% mkRMulSize $1 $3 $ toInt $3 }
    | SizeExpr '/' Integer                  {% mkDivSize $1 $3 $ toInt $3 }
    | '(' SizeExpr ')'                      {  $2 }
    | SizeIdent                             {  fmap VarSize $1 }
    | Integer           %prec SIZE_INT      {% mkSizeInt $1 $ toInt $1 }

{

toIdent (T start (TokIdent x) end) = fromLoc start x end
-- toIdent x = toIdent x

toSizeIdent (T start (TokSizeIdent x) end) = fromLoc start x end
-- toSizeIdent x = toSizeIdent x

toInteger (T _ (TokInteger x) _) = x
-- toInteger x = toInteger x

toInt tok@(T _ (TokInteger x) _) = case toIntegralSized x of
  Just i -> return i
  Nothing ->
    let msg = printf "Integer overflow: %d does not fit into Int." x
     in raiseError tok msg
-- toInt x = toInt x

}
