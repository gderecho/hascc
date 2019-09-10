module TestLexer where

import Test.HUnit.Base as HB
import Lexer

test1 = TestCase $ HB.assertEqual
    "Not implemented yet"
    True False
    

tests = TestList [TestLabel "Test 1" test1]
