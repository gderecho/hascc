module Main where

import Lexer
import Generator

input_to_asm :: String -> String
input_to_asm x = program x

main :: IO ()
main = interact input_to_asm
