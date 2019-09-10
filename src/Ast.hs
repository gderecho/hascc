module Ast where

data Primitive =
    PInteger
    deriving (Show,Eq)

data Val = Val Primitive String
    deriving (Show,Eq)

data Identifier = Identifier String
    deriving (Show,Eq)


data Expression = 
    BinaryOperator Operator Expression Expression |
    Literal Val                                   |
    Function String [Expression]                  |
    Return Val 
    deriving (Show,Eq)

type Condition = Expression

data Statement =
    SExpression Expression                                  |
    SCompound [Statement]                                   |

    SIf Condition Statement                        |
    SIfElse Condition Statement Statement          |

    SWhile Condition Statement                     |
    SDoWhile Statement Condition                   |
    SFor Statement Condition Expression Statement  |

    SBreak                                                  |
    SContinue                                               |
    SReturn Expression                                      |
    SGoto Identifier                                        |

    SDeclaration
    deriving (Show,Eq)
    

data LStatement =
    LStatement Identifier Statement           |
    LCase Identifier Statement                |
    LDefault Identifier Statement             
    

data Operator = 
    Plus  | 
    Minus |
    Times |
    Divide
    deriving (Show,Eq)
