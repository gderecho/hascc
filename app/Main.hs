module Main where

import Lexer
import Parser
import Generator
import ByteGenerator
import Data.Either (fromRight)

input_to_asm :: String -> String
input_to_asm x = 
    case parsed of 
        Right a -> generator . byte_generator $ a
        Left b -> error "Parser error"
    where parsed = parseAll x

main :: IO ()
main = do
    interact input_to_asm
