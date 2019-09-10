module Main where

import Lexer
import Generator
import ByteGenerator

input_to_asm :: String -> String
input_to_asm x = program x

main :: IO ()
main = do
    interact input_to_asm
