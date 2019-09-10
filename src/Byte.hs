module Byte
    where

-- Hascc's custom bytecode
--
--
--

data Register =
    Rax  |
    Rbx  |
    Rcx  |
    Rdx  |
    Rsp  |
    Rbp  |
    Rsi  |
    Rdi  |
    R8   |
    R9   |
    R10  |
    R11  |
    R12  |
    R13  |
    R14  |
    R15  
    deriving (Show,Eq)

type Comment = String

type Label = String

data Instruction =
    Mov Register Register    |
    Add Register Register    |
    Sub Register Register    |
    Cmp Register Register    |
    Push Register            |
    Pop  Register            |
    Inc  Register            |
    Neg  Register            |
    Not  Register            |
    Jmp  Label               |
    Je   Label               |
    Jne  Label               |
    Js   Label               |
    Jns  Label               |
    Jg   Label               |
    Jge  Label               |
    Jl   Label               |
    Jle  Label               |
    Ja   Label               |
    Jae  Label               |
    Jb   Label               |
    Jbe  Label               |
    Call Label               |
    Ret                      
    deriving (Show,Eq)

type ByteLine = (Maybe Label, Maybe Instruction, Maybe Comment)

type ByteProgram = [ByteLine]
