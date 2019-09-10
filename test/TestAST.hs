
module TestAST where

import Test.HUnit.Base as HB
import Ast

test1 = TestCase $ HB.assertEqual 
    ("Primitives")
    (PInteger) 
    (PInteger)

test2 = TestCase $ HB.assertEqual
    ("Expressions")
    (BinaryOperator Plus
        (Literal $ Val PInteger "23")
            (Literal $ Val PInteger "25"))
    (BinaryOperator Plus
        (Literal $ Val PInteger "23")
            (Literal $ Val PInteger "25"))

test3 = TestCase $ HB.assertBool
    "Expressions"
    $ Literal (Val PInteger "123") /= Literal (Val PInteger "124")

test4 = TestCase $ HB.assertBool
    "Plus"
    $ (Plus == Plus) &&
    (show Plus == "Plus")

test5 = TestCase $ HB.assertBool
    "Minus"
    $ (Minus == Minus) &&
    (show Minus == "Minus")

test6 = TestCase $ HB.assertEqual
    "Expression"
    (Function "Hello" [])
    (Function "Hello" [])

test7 = TestCase $ HB.assertBool
    "Expression"
    $ Function "Hello" [] /=
    Function "Hello" [Literal $ 
        Val PInteger "32"]



tests = TestList [TestLabel "Primitive Test" test1,
         TestLabel "Expression Test" test2,
         TestLabel "Expression Not Equals Test" test3,
        TestLabel "Plus" test4,
        TestLabel "Minus" test5,
        TestLabel "Expression" test6,
        TestLabel "Expression" test7
    ]
