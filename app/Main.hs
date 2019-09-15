module Main where

import Lexer
import Parser
import Generator
import ByteGenerator
import Data.Either (fromRight)

input_to_asm :: String -> String
input_to_asm = generator . byte_generator . fromRight [] . parseAll

main :: IO ()
main = do
    interact input_to_asm
