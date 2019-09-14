module ByteGenerator where

import Byte
import Ast



definition :: Statement -> [ByteLine]
definition (SFnDefinition 
                ([SQP PInt],[(NPD (DFunction (DId fname) []))],[]) 
                (SCompound [SReturn (Literal (Val PInt x))])
            )
    = [(Just fname,Nothing,Nothing),
       (Nothing,Just (Mov Rax (VL x)),Just "The return value")]

definition _ = error "Definition -- Not implemented"

byte_generator :: [Statement] -> ByteProgram
byte_generator [x] =
    definition x

byte_generator _ = error "Statement -- Not implemented"


