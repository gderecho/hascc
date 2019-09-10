import TestAST
import Test.QuickCheck
import Test.HUnit.Base as HB
import Test.HUnit
import System.Exit

main :: IO ()
main = do
    -- counts <- sequence $ fmap Test.HUnit.runTestTT TestAST.tests
    counts <- Test.HUnit.runTestTT TestAST.tests
    
    if (errors counts + failures counts == 0)
        then
            exitWith ExitSuccess
        else
            exitWith $ ExitFailure 1

    --if (sum (map errors counts) + sum (map failures counts) == 0)
    --    then
    --        exitWith ExitSuccess
    --    else
    --        exitWith $ ExitFailure 1
