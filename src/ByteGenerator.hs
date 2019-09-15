module ByteGenerator where

import Byte
import Ast

type Offset = Integer

call :: Statement -> [ByteLine]
call _ = error "Call not implemented yet"


-- SDecl ([SQP PInt],[NPD (DId "i")],[])



get_int_declarations :: [Statement] -> [Identifier]
get_int_declarations xs = [x | SDecl ([SQP PInt],[NPD (DId x)],_) <- xs ]

var_size :: [Statement] -> Integer
var_size = sum . map (\x -> 8) . get_int_declarations

var_offsets_help :: [Identifier] -> [(String,Offset)]
var_offsets_help [] = []
var_offsets_help (x:xs) = (x,0):(map (\(x,y) -> (x,y+8)) . var_offsets_help $ xs)

var_offsets :: [Statement] -> [(String, Offset)]
var_offsets = var_offsets_help . get_int_declarations
    

find_var :: String -> [(String,Offset)] -> Offset
find_var str xs = snd $ list!!0
    where list = filter (\(x,y) -> x == str) xs

registers_to_save :: [Register]
registers_to_save = [Rbx,Rbp,R12,R13,R14,R15]

save_registers :: [ByteLine]
save_registers = map (\x -> 
                    (Nothing, Just (Push x), Just ("Save "++ show x))
                ) registers_to_save

pop_saved_registers :: [ByteLine]
pop_saved_registers = map (\x -> 
                    (Nothing, Just (Pop x), Just ("Restore "++ show x))
                ) . reverse $ registers_to_save


ret_exp :: Integer -> Expression -> [ByteLine]
ret_exp stack_size (Literal (Val PInt x)) =
    concat [
        [
            (Nothing, Just (Mov Rax (VL x)), Just "The return value"),
            (Nothing, Nothing, Nothing)
        ],
        pop_saved_registers,
        [
            (Nothing, Just (Add Rsp (VL (show stack_size))), Nothing), -- Deallocate space
            (Nothing, Just Ret, Nothing)
        ]
    ]
ret_exp _ _ = error "Return -- not implemented"


-- p_statement: Processes a statement within a function
-- Parameter 1: The stack size
-- Parameter 2: The list of variable identifiers and their offsets
-- Parameter 3: The statement to process
p_statement :: Integer -> [(String,Offset)] -> Statement -> [ByteLine]

p_statement stack_size _ (SReturn expr) = ret_exp stack_size expr

p_statement _ _ x  
    | (get_int_declarations (return x)) /= [] = [] -- ignore int declaration
    | otherwise = error "p_statement -- not implemented"





fdefinition :: Statement -> [ByteLine]
fdefinition (SFnDefinition 
                ([SQP PInt],[(NPD (DFunction (DId fname) []))],[]) 
                (SCompound xs)
                -- (SCompound [SReturn (Literal (Val PInt x))])
            )
    = concat 
        [
            [
                (Just fname,Nothing,Nothing),
                (Nothing, Just (Sub Rsp (VL (show stack_size))), Just "Allocate space")
            ],
            save_registers,
            [
                (Nothing, Nothing,Nothing),
                (Nothing, Just (Push Rax),Nothing),
                (Nothing, Just (Xor Rax Rax),Nothing),
                (Nothing, Just (Mov Rdi (VL "hello")),Nothing),
                (Nothing, Just (Call "printf"),Nothing),
                (Nothing, Just (Pop Rax),Nothing),
                (Nothing, Nothing,Nothing)
            ],
            concat $ map (p_statement stack_size vars) $ xs
        ]
    where
        stack_size = var_size xs
        vars = var_offsets xs
        

fdefinition _ = error "Definition -- Not implemented"


byte_generator :: [Statement] -> ByteProgram
byte_generator = concat . map fdefinition


