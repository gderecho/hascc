module ByteGenerator where

import Lexer
import Ast


program :: String -> String
program x = unlines [
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
        "mov rax, " ++ x,
        "",
        "pop rbp",
        "pop rbx",
        "add rsp, 8",
        "ret"
    ]


-- generator :: String -> String
-- generator = program 


