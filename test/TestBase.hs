import Test.QuickCheck
import Test.HUnit.Base as HB
import Test.HUnit
import System.Exit

import TestAST
import TestGenerator
import TestParser
import TestLexer


list = [TestAST.tests, TestGenerator.tests, 
    TestLexer.tests, TestParser.tests]


-- main_help :: test -> IO ()
main_help test = do
    counts <- Test.HUnit.runTestTT test
    
    if (errors counts + failures counts /= 0)
        then exitWith $ ExitFailure 1
        else return ()

main :: IO ()
main = do
    sequence $ fmap main_help list
    exitWith ExitSuccess

