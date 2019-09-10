module Ast where

data Primitive =
    PInteger
    deriving (Show,Eq)

data Val = Val Primitive String
    deriving (Show,Eq)

data Expression = 
    BinaryOperator Operator Expression Expression |
    Literal Val |
    Function String [Expression] |
    Return Val 
    deriving (Show,Eq)
    


data Operator = 
    Plus | 
    Minus |
    Times |
    Divide
    deriving (Show,Eq)
