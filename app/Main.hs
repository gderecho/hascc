module Main where

import Lexer

program = unlines [
    "SECTION .DATA",
        "hello:      db \"Hello, world!\", 10, 0",
        "hello_len:  equ $-hello",
        "",
    "SECTION .TEXT",
        "GLOBAL main",
        "EXTERN printf",
        "",
    "main:",
        "sub rsp, 8 ",
        "push rbx",
        "push rbp",
         " ",
        "mov rdi, hello",
        "call printf",
        "",
        "pop rbp",
        "pop rbx",
        "add rsp, 8",
        "ret"
    ]

input_to_asm :: String -> String
input_to_asm x = program

main :: IO ()
main = interact input_to_asm
