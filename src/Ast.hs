module Ast where

data Primitive =
    Primitive Integer

data Expression = 
    BinaryOperator Operator Expression Expression |
    Var Primitive String |
    Function String [Expression]
    


data Operator = 
    Plus | 
    Minus |
    Times |
    Divide
    deriving (Eq,Ord,Show)
