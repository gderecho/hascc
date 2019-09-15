module Generator where

import Byte


-- Generates x86-64 NASM
--
--
-- TODO

program :: String -> String -> String
program s t = unlines [
    "SECTION .DATA",
        "hello:      db \"Hello, world!\", 10, 0",
        "hello_len:  equ $-hello",
        "",
    "SECTION .TEXT",
        "GLOBAL main",
        "EXTERN printf",
        "",
        s, -- function name
        "sub rsp, 8 ",
        "push rbx",
        "push rbp",
         " ",
        "mov rdi, hello",
        "call printf",
        "",
        t, -- return value
        "",
        "pop rbp",
        "pop rbx",
        "add rsp, 8",
        "ret"
    ]

generator :: ByteProgram -> String
generator [x,y]
    = program (btasm x) (btasm y)
generator _ = "Generator -- Not implemented yet"

