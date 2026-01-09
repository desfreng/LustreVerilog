{
module Parsing.Lexer where

import Parsing.Commons
import Parsing.LexAction
import Commons.Types (BitVectorKind(..))

}

%encoding "utf8"

$binDigit = [0-1]
$octDigit = [0-7]
$decDigit = [0-9]
$hexDigit = [0-9a-fA-F]

$lower = [a-z]
$upper = [A-Z]
$ident  = [$lower $upper $decDigit \_ ']

tokens :-

  -- Whitespace and Comments
  $white+                         ;
  "/*" [.\n]* "*/"                ;
  "//" .*                         ;

  -- Specific Type Shortcuts (r<8>, u<16>, i<32> etc)
  r                               { mkAtomic TokR }
  u                               { mkAtomic TokU }
  i                               { mkAtomic TokI }

  -- Identifiers and Keywords
  $lower $ident*                  { mkIdent }
  $upper $ident*                  { mkSizeVar }

  -- Symbols
  "("                             { mkAtomic TokLParen }
  ")"                             { mkAtomic TokRParen }
  "["                             { mkAtomic TokLBrack }
  "]"                             { mkAtomic TokRBrack }
  ","                             { mkAtomic TokComma }
  ";"                             { mkAtomic TokSemi }
  ":"                             { mkAtomic TokColon }
  "="                             { mkAtomic TokEq }
  "=="                            { mkAtomic TokEqEq }
  "<>"                            { mkAtomic TokNeq }
  ">="                            { mkAtomic TokGe }
  "<="                            { mkAtomic TokLe }
  ">"                             { mkAtomic TokGt }
  "<"                             { mkAtomic TokLt }
  "+"                             { mkAtomic TokPlus }
  "-"                             { mkAtomic TokMinus }
  "*"                             { mkAtomic TokAst }
  "/"                             { mkAtomic TokSlash }
  "++"                            { mkAtomic TokPlusPlus }

  -- Numeric Literals
  0x $hexDigit+                   { mkInt Hexadecimal }
  0o $octDigit+                   { mkInt Octal }
  0d $decDigit+                   { mkInt Decimal }
  0b $decDigit+                   { mkInt Binary }
  $decDigit+                      { mkInt Unspecified }

{

}

