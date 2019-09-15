module TestGenerator where

import Test.HUnit.Base as HB
import Generator

test2 = TestCase $ HB.assertEqual
    "Not implemented yet"
    True False

tests = TestList [TestLabel "Test 1" test1]
