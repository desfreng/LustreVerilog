{-# LANGUAGE OverloadedStrings #-}

module Parsing.ParserSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Parsing.Parser
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, parse)

loc :: Int -> Int -> a -> Localized a
loc beg end x = L beg x end

pEnd :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
pEnd x = parse (x <* eof) ""

p :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
p x = pEnd $ spaceConsumer *> x

spec :: Spec
spec = do
  spaceConsumerSpec
  pTypeSpec
  pConstantSpec
  pPatternSpec
  pExprSpec
  pEquationSpec
  pDeclSpec
  pNodeSpec
  pFileSpec

spaceConsumerSpec :: Spec
spaceConsumerSpec =
  describe "spaceConsumer" $ do
    it "can skip space" $ do
      pEnd spaceConsumer `shouldSucceedOn` " \t\n\r "

    it "can skip comment" $ do
      pEnd spaceConsumer `shouldSucceedOn` " /* Hello\n\nWorld! */ "

    it "fail on nested comment" $ do
      pEnd spaceConsumer `shouldFailOn` " /* Hello\n/* Hi */\nWorld! */ "

    it "fail on not spec" $ do
      pEnd spaceConsumer `shouldFailOn` "\n\rH "

pConstantSpec :: Spec
pConstantSpec =
  describe "pConstant" $ do
    describe "integer" $ do
      it "can parse decimal integer" $ do
        p pConstant "1024" `shouldParse` loc 0 4 (IntegerConst 1024)
        p pConstant "01024" `shouldParse` loc 0 5 (IntegerConst 1024)
        p pConstant "0" `shouldParse` loc 0 1 (IntegerConst 0)
        p pConstant "0d1024" `shouldParse` loc 0 6 (IntegerConst 1024)

      it "can parse hexadecimal integer" $ do
        p pConstant "0x1024" `shouldParse` loc 0 6 (IntegerConst 0x1024)

      it "can parse octal integer" $ do
        p pConstant "0o1024" `shouldParse` loc 0 6 (IntegerConst 0o1024)

      it "can parse binary integer" $ do
        p pConstant "0b1010" `shouldParse` loc 0 6 (IntegerConst 10)

    describe "boolean" $ do
      it "can parse true" $ do
        p pConstant "true" `shouldParse` loc 0 4 (BoolConst True)

      it "can parse false" $ do
        p pConstant "false" `shouldParse` loc 0 5 (BoolConst False)

pTypeSpec :: Spec
pTypeSpec =
  describe "pType" $ do
    it "can parse int type" $ do
      p pType "int " `shouldParse` loc 0 3 IntegerType

    it "can parse false" $ do
      p pType " bool " `shouldParse` loc 1 5 BoolType

pPatternSpec :: Spec
pPatternSpec =
  describe "pPattern" $ do
    it "can parse simple pattern" $ do
      p pPattern "pat   " `shouldParse` (PatIdent . loc 0 3 $ Ident "pat")
      p pPattern "  pat   " `shouldParse` (PatIdent . loc 2 5 $ Ident "pat")
      p pPattern "  pat_e1325Xe \n"
        `shouldParse` (PatIdent . loc 2 13 $ Ident "pat_e1325Xe")

    it "can parse composite pattern" $ do
      p pPattern " (x, (t, y), g)  "
        `shouldParse` PatTuple
          ( BiList
              (PatIdent . loc 2 3 $ Ident "x")
              ( PatTuple $
                  BiList (PatIdent . loc 6 7 $ Ident "t") (PatIdent . loc 9 10 $ Ident "y") []
              )
              [PatIdent . loc 13 14 $ Ident "g"]
          )

      p pPattern " (xg)  " `shouldParse` (PatIdent . loc 2 4 $ Ident "xg")

pExprSpec :: Spec
pExprSpec =
  describe "pExpr" $ do
    it "can parse constants" $ do
      p pExpr " 01024 " `shouldParse` loc 1 6 (ConstantExpr $ IntegerConst 1024)
      p pExpr " false " `shouldParse` loc 1 6 (ConstantExpr $ BoolConst False)
      p pExpr " true " `shouldParse` loc 1 5 (ConstantExpr $ BoolConst True)

    it "can parse identifiers" $ do
      p pExpr " hello\n\n  " `shouldParse` loc 1 6 (IdentExpr . loc 1 6 $ Ident "hello")
      p pExpr " nota " `shouldParse` loc 1 5 (IdentExpr . loc 1 5 $ Ident "nota")
      p pExpr " preb " `shouldParse` loc 1 5 (IdentExpr . loc 1 5 $ Ident "preb")
      p pExpr " ifb " `shouldParse` loc 1 4 (IdentExpr . loc 1 4 $ Ident "ifb")
      p pExpr " thenb " `shouldParse` loc 1 6 (IdentExpr . loc 1 6 $ Ident "thenb")

    it "can parse tuples" $ do
      p pExpr " (a, b,    10,   c)    "
        `shouldParse` loc
          1
          19
          ( TupleExpr $
              BiList
                (loc 2 3 . IdentExpr . loc 2 3 $ Ident "a")
                (loc 5 6 . IdentExpr . loc 5 6 $ Ident "b")
                [ loc 11 13 . ConstantExpr $ IntegerConst 10,
                  loc 17 18 . IdentExpr . loc 17 18 $ Ident "c"
                ]
          )

      p pExpr " ( a ) " `shouldParse` loc 3 4 (IdentExpr . loc 3 4 $ Ident "a")

    it "can parse function calls" $ do
      p pExpr " f ( x , 10 )   "
        `shouldParse` loc
          1
          13
          ( AppExpr
              (loc 1 2 $ Ident "f")
              [ loc 5 6 . IdentExpr . loc 5 6 $ Ident "x",
                loc 9 11 . ConstantExpr $ IntegerConst 10
              ]
          )

      p pExpr " send_halt  (  )   "
        `shouldParse` loc
          1
          16
          (AppExpr (loc 1 10 $ Ident "send_halt") [])

    it "can parse unary expression" $ do
      p pExpr " -10 " `shouldParse` loc 1 4 (UnOpExpr UnMinus . loc 2 4 . ConstantExpr $ IntegerConst 10)

      p pExpr " +1 " `shouldParse` loc 1 3 (ConstantExpr $ IntegerConst 1)

      p pExpr " not  a " `shouldParse` loc 1 7 (UnOpExpr UnNot . loc 6 7 . IdentExpr . loc 6 7 $ Ident "a")
      p pExpr " not\na " `shouldParse` loc 1 6 (UnOpExpr UnNot . loc 5 6 . IdentExpr . loc 5 6 $ Ident "a")

      p pExpr "  not  - a "
        `shouldParse` loc 2 10 (UnOpExpr UnNot . loc 7 10 . UnOpExpr UnMinus . loc 9 10 . IdentExpr . loc 9 10 $ Ident "a")

    it "can parse binary expression" $ do
      p pExpr " 10 + a "
        `shouldParse` loc
          1
          7
          (BinOpExpr BinAdd (loc 1 3 . ConstantExpr $ IntegerConst 10) (loc 6 7 . IdentExpr . loc 6 7 $ Ident "a"))

      p pExpr " a - b + c "
        `shouldParse` loc
          1
          10
          ( BinOpExpr
              BinAdd
              ( loc 1 6
                  . BinOpExpr
                    BinSub
                    (loc 1 2 . IdentExpr . loc 1 2 $ Ident "a")
                  $ loc 5 6 . IdentExpr . loc 5 6
                  $ Ident "b"
              )
              . loc 9 10
              . IdentExpr
              . loc 9 10
              $ Ident "c"
          )

      p pExpr " a ==  b and  b <> c  "
        `shouldParse` loc
          1
          20
          ( BinOpExpr
              BinAnd
              ( loc 1 8
                  . BinOpExpr
                    BinEq
                    (loc 1 2 . IdentExpr . loc 1 2 $ Ident "a")
                  $ loc 7 8 . IdentExpr . loc 7 8
                  $ Ident "b"
              )
              . loc 14 20
              . BinOpExpr
                BinNeq
                (loc 14 15 . IdentExpr . loc 14 15 $ Ident "b")
              $ loc 19 20 . IdentExpr . loc 19 20
              $ Ident "c"
          )

      p pExpr " a != b or b >= c fby a < c "
        `shouldParse` loc
          1
          27
          ( FbyExpr
              ( loc 1 17
                  . BinOpExpr
                    BinOr
                    ( loc 1 7
                        . BinOpExpr
                          BinNeq
                          (loc 1 2 . IdentExpr . loc 1 2 $ Ident "a")
                        $ loc 6 7 . IdentExpr . loc 6 7
                        $ Ident "b"
                    )
                  . loc
                    11
                    17
                  . BinOpExpr BinGe (loc 11 12 . IdentExpr . loc 11 12 $ Ident "b")
                  . loc 16 17
                  . IdentExpr
                  . loc 16 17
                  $ Ident "c"
              )
              . loc 22 27
              . BinOpExpr
                BinLt
                (loc 22 23 . IdentExpr . loc 22 23 $ Ident "a")
              $ loc 26 27 . IdentExpr . loc 26 27
              $ Ident "c"
          )

      p pExpr "  a=b and b<=c or a>not c "
        `shouldParse` loc
          2
          25
          ( BinOpExpr
              BinOr
              ( loc 2 14
                  . BinOpExpr
                    BinAnd
                    ( loc 2 5
                        . BinOpExpr
                          BinEq
                          (loc 2 3 . IdentExpr . loc 2 3 $ Ident "a")
                        $ loc 4 5 . IdentExpr . loc 4 5
                        $ Ident "b"
                    )
                  . loc
                    10
                    14
                  . BinOpExpr BinLe (loc 10 11 . IdentExpr . loc 10 11 $ Ident "b")
                  . loc 13 14
                  . IdentExpr
                  . loc 13 14
                  $ Ident "c"
              )
              . loc 18 25
              . BinOpExpr
                BinGt
                (loc 18 19 . IdentExpr . loc 18 19 $ Ident "a")
              . loc 20 25
              . UnOpExpr UnNot
              . loc 24 25
              . IdentExpr
              . loc 24 25
              $ Ident "c"
          )

    it "can parse if expression" $ do
      p pExpr "  if a then b else c  "
        `shouldParse` loc
          2
          20
          ( IfExpr
              (loc 5 6 . IdentExpr . loc 5 6 $ Ident "a")
              (loc 12 13 . IdentExpr . loc 12 13 $ Ident "b")
              . loc 19 20
              . IdentExpr
              . loc 19 20
              $ Ident "c"
          )
      p pExpr "  if 1+2then 0fby 1else\n1or 2   "
        `shouldParse` loc
          2
          29
          ( IfExpr
              ( loc 5 8
                  . BinOpExpr
                    BinAdd
                    (loc 5 6 . ConstantExpr $ IntegerConst 1)
                  . loc 7 8
                  . ConstantExpr
                  $ IntegerConst 2
              )
              ( loc 13 19
                  . FbyExpr
                    (loc 13 14 . ConstantExpr $ IntegerConst 0)
                  . loc 18 19
                  . ConstantExpr
                  $ IntegerConst 1
              )
              . loc 24 29
              . BinOpExpr
                BinOr
                (loc 24 25 . ConstantExpr $ IntegerConst 1)
              . loc 28 29
              . ConstantExpr
              $ IntegerConst 2
          )

pEquationSpec :: Spec
pEquationSpec =
  describe "pEquation" $
    it "can parse equations" $ do
      p pEquation " x=1 ;"
        `shouldParse` Equation
          (PatIdent . loc 1 2 $ Ident "x")
          (loc 3 4 . ConstantExpr $ IntegerConst 1)

      p pEquation " (x, y)  =  (y,\nx) ; "
        `shouldParse` Equation
          ( PatTuple $
              BiList
                (PatIdent . loc 2 3 $ Ident "x")
                (PatIdent . loc 5 6 $ Ident "y")
                []
          )
          ( loc 12 18
              . TupleExpr
              $ BiList
                (loc 13 14 . IdentExpr . loc 13 14 $ Ident "y")
                (loc 16 17 . IdentExpr . loc 16 17 $ Ident "x")
                []
          )

pDeclSpec :: Spec
pDeclSpec =
  describe "pDecl" $ do
    it "can parse declaration" $
      p pDecl " a,   b,  c  :  bool  "
        `shouldParse` ( (IdentDecl (loc 1 2 $ Ident "a") . loc 16 20 $ BoolType)
                          :| [ IdentDecl (loc 6 7 $ Ident "b") . loc 16 20 $ BoolType,
                               IdentDecl (loc 10 11 $ Ident "c") . loc 16 20 $ BoolType
                             ]
                      )

pNodeSpec :: Spec
pNodeSpec =
  describe "pNode" $
    it "can parse Node" $ do
      p pNode `shouldFailOn` "nodex"
      p pNode `shouldFailOn` "node x()returns() b"
      p pNode `shouldFailOn` "node x()returns();vare"
      p pNode `shouldFailOn` "node x()returns();var e:bool;lete"
      p pNode `shouldFailOn` "node x()returns();lete"
      p pNode `shouldFailOn` "node x()returns();let x=x;tele"
      p pNode `shouldFailOn` "node x () returns (); let x=x; tel" -- No return variables
      p pNode " node x () returns (x: int); let a=1; tel "
        `shouldParse` loc
          1
          41
          ( Node
              (loc 6 7 $ Ident "x")
              []
              (IdentDecl (loc 20 21 $ Ident "x") (loc 23 26 IntegerType) :| [])
              []
              [Equation (PatIdent . loc 33 34 $ Ident "a") . loc 35 36 . ConstantExpr $ IntegerConst 1]
          )

      p pNode " node x(a:bool)returns(o:bool);let b=1;o=a; tel  "
        `shouldParse` loc
          1
          47
          ( Node
              (loc 6 7 $ Ident "x")
              [IdentDecl (loc 8 9 $ Ident "a") . loc 10 14 $ BoolType]
              ((IdentDecl (loc 23 24 $ Ident "o") . loc 25 29 $ BoolType) :| [])
              []
              [ Equation (PatIdent . loc 35 36 $ Ident "b") . loc 37 38 . ConstantExpr $ IntegerConst 1,
                Equation (PatIdent . loc 39 40 $ Ident "o") . loc 41 42 . IdentExpr . loc 41 42 $ Ident "a"
              ]
          )

      p pNode " node x()returns(x:int);var a,b:bool;\na,d:int;let x=x; tel  "
        `shouldParse` loc
          1
          58
          ( Node
              (loc 6 7 $ Ident "x")
              []
              (IdentDecl (loc 17 18 $ Ident "x") (loc 19 22 IntegerType) :| [])
              [ IdentDecl (loc 28 29 $ Ident "a") . loc 32 36 $ BoolType,
                IdentDecl (loc 30 31 $ Ident "b") . loc 32 36 $ BoolType,
                IdentDecl (loc 38 39 $ Ident "a") . loc 42 45 $ IntegerType,
                IdentDecl (loc 40 41 $ Ident "d") . loc 42 45 $ IntegerType
              ]
              [ Equation (PatIdent . loc 50 51 $ Ident "x") . loc 52 53 . IdentExpr . loc 52 53 $ Ident "x"
              ]
          )

pFileSpec :: Spec
pFileSpec =
  describe "pFile" $
    it "can parse Lustre file" $
      p pFile "  node x   () \n\t returns  (x:int)  ;   let  x = x  ;  tel\n node y   () \n\t returns   (y:bool); let y = y \n;  tel "
        `shouldParse` Ast
          [ loc 2 57 $
              Node
                (loc 7 8 $ Ident "x")
                []
                (IdentDecl (loc 27 28 $ Ident "x") (loc 29 32 IntegerType) :| [])
                []
                [ Equation
                    (PatIdent . loc 44 45 $ Ident "x")
                    ( loc 48 49
                        . IdentExpr
                        . loc 48 49
                        $ Ident "x"
                    )
                ],
            loc 59 111 $
              Node
                (loc 64 65 $ Ident "y")
                []
                (IdentDecl (loc 85 86 $ Ident "y") (loc 87 91 BoolType) :| [])
                []
                [ Equation
                    (PatIdent . loc 98 99 $ Ident "y")
                    ( loc 102 103 . IdentExpr . loc 102 103 $
                        Ident "y"
                    )
                ]
          ]