module Ast where

data Primitive =
    PInteger
    deriving (Show,Eq)

data Expression = 
    BinaryOperator Operator Expression Expression |
    Val Primitive String |
    Function String [Expression]
    deriving (Show,Eq)
    


data Operator = 
    Plus | 
    Minus |
    Times |
    Divide
    deriving (Show,Eq)
