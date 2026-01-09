{-# LANGUAGE InstanceSigs #-}

module Parsing.Commons
  ( LexPos (..),
    lexToPos,
    movePos,
    TokenTag (..),
    Token (..),
  )
where

import Commons.Ids (Ident, SizeIdent)
import Commons.Position (Location (..), Pos (), fromLoc)
import Data.Word (Word8)
import Text.Printf

data LexPos
  = LexPos
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Location

lexToPos :: LexPos -> a -> LexPos -> Pos a
lexToPos (LexPos _ start) x (LexPos _ end) = fromLoc start x end

newLine :: Word8
newLine = 0x0A

movePos :: LexPos -> Word8 -> LexPos
movePos (LexPos a (Location l c)) x =
  let newL =
        if x == newLine
          then Location (l + 1) 1
          else Location l (c + 1)
   in LexPos (a + 1) newL

data TokenTag
  = TokTrue
  | TokFalse
  | TokBool
  | TokIntKw
  | TokRaw
  | TokSigned
  | TokUnsigned
  | TokR
  | TokI
  | TokU
  | TokIf
  | TokThen
  | TokElse
  | TokNot
  | TokAnd
  | TokOr
  | TokFby
  | TokNode
  | TokReturns
  | TokSize
  | TokWhere
  | TokVar
  | TokLet
  | TokTel
  | TokWhen
  | TokOtherwise
  | TokLParen
  | TokRParen
  | TokLBrack
  | TokRBrack
  | TokComma
  | TokSemi
  | TokColon
  | TokEq
  | TokEqEq
  | TokNeq
  | TokGe
  | TokLe
  | TokGt
  | TokLt
  | TokPlus
  | TokMinus
  | TokAst
  | TokSlash
  | TokPlusPlus
  | TokIdent {-# UNPACK #-} !Ident
  | TokSizeIdent {-# UNPACK #-} !SizeIdent
  | TokInteger {-# UNPACK #-} !Integer
  | TokEOF

data Token
  = T
      {-# UNPACK #-} !Location
      {-# UNPACK #-} !TokenTag
      {-# UNPACK #-} !Location

instance Show TokenTag where
  show :: TokenTag -> String
  show TokTrue = "true"
  show TokFalse = "false"
  show TokBool = "bool"
  show TokIntKw = "int"
  show TokRaw = "raw"
  show TokSigned = "signed"
  show TokUnsigned = "unsigned"
  show TokR = "r"
  show TokI = "i"
  show TokU = "u"
  show TokIf = "if"
  show TokThen = "then"
  show TokElse = "else"
  show TokNot = "not"
  show TokAnd = "and"
  show TokOr = "or"
  show TokFby = "fby"
  show TokNode = "node"
  show TokReturns = "returns"
  show TokSize = "size"
  show TokWhere = "where"
  show TokVar = "var"
  show TokLet = "let"
  show TokTel = "tel"
  show TokWhen = "when"
  show TokOtherwise = "otherwise"
  show TokLParen = "("
  show TokRParen = ")"
  show TokLBrack = "["
  show TokRBrack = "]"
  show TokComma = ","
  show TokSemi = ";"
  show TokColon = ":"
  show TokEq = "="
  show TokEqEq = "=="
  show TokNeq = "<>"
  show TokGe = ">="
  show TokLe = "<="
  show TokGt = ">"
  show TokLt = "<"
  show TokPlus = "+"
  show TokMinus = "-"
  show TokAst = "*"
  show TokSlash = "/"
  show TokPlusPlus = "++"
  show (TokIdent t) = printf "<id=%s>" $ show t
  show (TokSizeIdent t) = printf "<sId=%s>" $ show t
  show (TokInteger d) = printf "<int=%d>" d
  show TokEOF = "eof"

instance Show Token where
  show :: Token -> String
  show (T _ tok _) = show tok

instance PrintfArg Token where
  formatArg :: Token -> FieldFormatter
  formatArg = formatString . show

instance PrintfArg TokenTag where
  formatArg :: TokenTag -> FieldFormatter
  formatArg = formatString . show
