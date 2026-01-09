{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.LexAction
  ( -- LexAction (..),
    LexAction (),
    runLexAction,
    -- Alex Stuff
    AlexInput (..),
    alexInputPrevChar,
    alexGetByte,
    initInput,
    -- Lexing Utils
    IntKind (..),
    mkIdent,
    mkInt,
    mkSizeVar,
    mkAtomic,
  )
where

import Commons.Ids (Ident (Ident), SizeIdent (SizeIdent))
import Commons.Position (Location (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Parsing.Commons (LexPos (..), Token (..), TokenTag (..), movePos)

-- Alex Stuff

data AlexInput = AlexInput
  { inputRest :: {-# UNPACK #-} !ByteString,
    currPos :: {-# UNPACK #-} !LexPos
  }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar is not defined."

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput {inputRest, currPos}) =
  case B.uncons inputRest of
    Nothing -> Nothing
    Just (c, rest) ->
      Just (c, AlexInput {inputRest = rest, currPos = movePos currPos c})

initInput :: ByteString -> AlexInput
initInput i = AlexInput i . LexPos 0 $ Location 1 1

-- Lexing Action
newtype LexAction a = LexAction
  {unwrapLexAction :: (Location, Text, Location) -> a}

wrapToken :: (Text -> TokenTag) -> LexAction Token
wrapToken f = LexAction $ \(start, txt, end) -> T start (f txt) end

runLexAction :: LexAction a -> Location -> ByteString -> Location -> a
runLexAction (LexAction act) start bTxt end = act (start, decodeUtf8 bTxt, end)

instance Functor LexAction where
  fmap :: (a -> b) -> LexAction a -> LexAction b
  fmap f (LexAction g) =
    LexAction $ f . g

instance Applicative LexAction where
  pure :: a -> LexAction a
  pure = LexAction . const

  (<*>) :: LexAction (a -> b) -> LexAction a -> LexAction b
  (LexAction f) <*> (LexAction x) =
    LexAction $ \i -> f i $ x i

instance Monad LexAction where
  (>>=) :: LexAction a -> (a -> LexAction b) -> LexAction b
  (LexAction x) >>= f =
    LexAction $ \i -> unwrapLexAction (f $ x i) i

readInt :: (Integral a) => a -> Text -> a
readInt base =
  T.foldl' (\acc c -> acc * base + fromIntegral (digitToInt c)) 0

parseBinary :: (Integral a) => Text -> a
parseBinary = readInt 2

parseOctal :: (Integral a) => Text -> a
parseOctal = readInt 8

parseDecimal :: (Integral a) => Text -> a
parseDecimal = readInt 10

parseHexadecimal :: (Integral a) => Text -> a
parseHexadecimal = readInt 16

data IntKind = Binary | Octal | Decimal | Hexadecimal | Unspecified

mkInt :: IntKind -> LexAction Token
mkInt kind = wrapToken go
  where
    go txt = TokInteger $ case kind of
      Binary -> let numPart = T.drop 2 txt in parseBinary numPart
      Octal -> let numPart = T.drop 2 txt in parseOctal numPart
      Decimal -> let numPart = T.drop 2 txt in parseDecimal numPart
      Hexadecimal -> let numPart = T.drop 2 txt in parseHexadecimal numPart
      Unspecified -> parseDecimal txt

keywords :: Map Text TokenTag
keywords =
  let keywordList =
        [ ("if", TokIf),
          ("or", TokOr),
          ("and", TokAnd),
          ("fby", TokFby),
          ("int", TokIntKw),
          ("let", TokLet),
          ("raw", TokRaw),
          ("tel", TokTel),
          ("var", TokVar),
          ("not", TokNot),
          ("bool", TokBool),
          ("else", TokElse),
          ("node", TokNode),
          ("size", TokSize),
          ("then", TokThen),
          ("true", TokTrue),
          ("when", TokWhen),
          ("false", TokFalse),
          ("where", TokWhere),
          ("signed", TokSigned),
          ("returns", TokReturns),
          ("unsigned", TokUnsigned),
          ("otherwise", TokOtherwise)
        ]
   in Map.fromList keywordList

mkIdent :: LexAction Token
mkIdent = wrapToken $ \txt ->
  fromMaybe (TokIdent $ Ident txt) $ Map.lookup txt keywords

mkSizeVar :: LexAction Token
mkSizeVar = wrapToken $ TokSizeIdent . SizeIdent . Ident

mkAtomic :: TokenTag -> LexAction Token
mkAtomic = wrapToken . const
