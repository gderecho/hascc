module ByteGenerator where

import Byte
import Ast



definition :: Statement -> [ByteLine]
definition (SFnDefinition 
                ([SQP PInt],[(NPD (DFunction (DId fname) []))],[]) 
                (SCompound [SReturn (Literal (Val PInt x))])
            )
    = [
           (Just fname,Nothing,Nothing),
           (Nothing, Just (Sub Rsp (VL "8")), Just "Allocate space for local variables"),
           (Nothing, Just (Push Rbx),Nothing),
           (Nothing, Just (Push Rbp),Nothing),
           (Nothing, Nothing,Nothing),
           (Nothing, Just (Mov Rdi (VL "hello")),Nothing),
           (Nothing, Just (Call "printf"),Nothing),
           (Nothing, Nothing,Nothing),
           (Nothing, Just (Mov Rax (VL x)), Just "The return value"),
           (Nothing, Nothing, Nothing),
           (Nothing, Just (Pop Rbp), Nothing),
           (Nothing, Just (Pop Rbx), Nothing),
           (Nothing, Just (Add Rsp (VL "8")), Nothing),
           (Nothing, Just Ret, Nothing)
    ]

definition _ = error "Definition -- Not implemented"

byte_generator :: [Statement] -> ByteProgram
byte_generator = concat . map definition



