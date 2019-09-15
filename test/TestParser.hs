module TestParser where

import Test.HUnit.Base as HB
import Parser

test1 = TestCase $ HB.assertEqual
    "Not implemented yet"
    True False
    

tests = TestList [TestLabel "Test 1" test1]
