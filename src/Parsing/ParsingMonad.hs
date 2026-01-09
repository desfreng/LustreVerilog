{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parsing.ParsingMonad
  ( ParsingMonad (),
    lexer,
    parseError,
    raiseError,
    ParsingParam (..),
    runParsingMonad,
    readParam,
  )
where

import Commons.Error (CanFail, reportLocatedError)
import Commons.Position (Pos (..), fromLoc)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Parsing.Commons (LexPos (..), Token (..), TokenTag (..))
import Parsing.LexAction (AlexInput (..), initInput, runLexAction)
import Parsing.Lexer (AlexReturn (..), alexScan)
import Text.Printf (printf)

newtype ParsingParam
  = ParsingParam {defaultIntSize :: Int}

newtype ParsingMonad a = ParsingMonad
  { unwrapPM :: (AlexInput, ParsingParam) -> Either (Pos String) (a, AlexInput)
  }

instance Functor ParsingMonad where
  fmap :: (a -> b) -> ParsingMonad a -> ParsingMonad b
  fmap f (ParsingMonad g) =
    ParsingMonad $ fmap (first f) . g

instance Applicative ParsingMonad where
  pure :: a -> ParsingMonad a
  pure x = ParsingMonad $ \(i, _) -> Right (x, i)

  (<*>) :: ParsingMonad (a -> b) -> ParsingMonad a -> ParsingMonad b
  (ParsingMonad f) <*> (ParsingMonad x) =
    ParsingMonad $ \(i, p) -> case f (i, p) of
      Left err -> Left err
      Right (h, i') -> first h <$> x (i', p)

instance Monad ParsingMonad where
  (>>=) :: ParsingMonad a -> (a -> ParsingMonad b) -> ParsingMonad b
  (ParsingMonad x) >>= f =
    ParsingMonad $ \(i, p) -> case x (i, p) of
      Left err -> Left err
      Right (y, i') -> unwrapPM (f y) (i', p)

lexer :: (Token -> ParsingMonad a) -> ParsingMonad a
lexer f = ParsingMonad $ \(st, p) ->
  let LexPos _ startPos = currPos st
   in case alexScan st 0 of
        AlexSkip newSt _ -> unwrapPM (lexer f) (newSt, p)
        AlexError errSt ->
          let LexPos _ endPos = errSt.currPos
           in Left $ fromLoc startPos "Lexing error" endPos
        AlexEOF ->
          unwrapPM (f $ T startPos TokEOF startPos) (st, p)
        AlexToken newSt len act ->
          let LexPos _ endPos = currPos newSt
              bTxt = B.take len $ inputRest st
              tok = runLexAction act startPos bTxt endPos
           in unwrapPM (f tok) (newSt, p)

raiseError :: Token -> String -> ParsingMonad a
raiseError (T start _ end) msg = ParsingMonad $ \_ ->
  Left $ fromLoc start msg end

parseError :: Token -> ParsingMonad a
parseError t@(T _ tok _) =
  let msg = printf "Unexpected Token: '%v'" tok
   in raiseError t msg

runParsingMonad :: ParsingMonad a -> ByteString -> ParsingParam -> CanFail a
runParsingMonad (ParsingMonad m) fdata p =
  let i = initInput fdata
   in either reportLocatedError (pure . fst) $ m (i, p)

readParam :: (ParsingParam -> a) -> ParsingMonad a
readParam f = ParsingMonad $ \(i, p) -> Right (f p, i)
