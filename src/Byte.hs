module Byte
    where

-- Hascc's custom bytecode
-- Right now, basically just assembler
--
--

import Data.Maybe (fromMaybe)
import Data.Char (toLower)

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
    deriving (Eq)

instance Show Register where
    show Rax = "rax"
    show Rbx = "rbx"
    show Rcx = "rcx"
    show Rdx = "rdx"
    show Rsp = "rsp"
    show Rbp = "rbp"
    show Rsi = "rsi"
    show Rdi = "rdi"
    show R8  = "r8"
    show R9  = "r9"
    show R10 = "r10"
    show R11 = "r11"
    show R12 = "r12"
    show R13 = "r13"
    show R14 = "r14"
    show R15 = "r15"

type Comment = String

type Label = String

data Value =
    VR Register |
    VL String
    deriving (Eq)

instance Show Value where
    show (VR x) = show x
    show (VL x) = show x

data Instruction =
    Mov Register Value       |
    Add Register Value       |
    Sub Register Value       |
    Cmp Register Register    |
    Xor Register Register    |
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
    deriving (Eq)

instance Show Instruction where
    show (Mov x y) = "mov " ++ show x ++ ", " ++ show y
    show (Add x y) = "add " ++ show x ++ ", " ++ show y
    show (Sub x y) = "sub " ++ show x ++ ", " ++ show y
    show (Cmp x y) = "cmp " ++ show x ++ ", " ++ show y
    show (Xor x y) = "xor " ++ show x ++ ", " ++ show y
    show (Push x ) = "push " ++ show x
    show (Pop  x ) = "pop " ++ show x
    show (Inc  x ) = "inc " ++ show x            
    show (Neg  x ) = "neg " ++ show x            
    show (Not  x ) = "not " ++ show x            
    show (Jmp  x ) = "jmp " ++ show x               
    show (Je   x ) = "je " ++ show x               
    show (Jne  x ) = "jne " ++ show x               
    show (Js   x ) = "js " ++ show x               
    show (Jns  x ) = "jns " ++ show x               
    show (Jg   x ) = "jg " ++ show x               
    show (Jge  x ) = "jge " ++ show x               
    show (Jl   x ) = "jl " ++ show x               
    show (Jle  x ) = "jle " ++ show x               
    show (Ja   x ) = "ja " ++ show x               
    show (Jae  x ) = "jae " ++ show x               
    show (Jb   x ) = "jb " ++ show x               
    show (Jbe  x ) = "jbe " ++ show x               
    show (Call x ) = "call " ++ show x               
    show Ret = "ret"


type ByteLine = (Maybe Label, Maybe Instruction, Maybe Comment)

type ByteProgram = [ByteLine]

btasm :: ByteLine -> String
btasm (x,y,z) =
    filter (/='\"') $ map toLower (
        (fromMaybe "" (fmap (\a -> a ++ ":") x))
        ++
        (fromMaybe "" $ fmap show y)
        ++
        " ; "
        ++
        (fromMaybe "" z)
    )

