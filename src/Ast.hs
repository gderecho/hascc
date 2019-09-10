module Ast where

data Primitive =
    PInteger
    deriving (Show,Eq)

data Val = Val Primitive String
    deriving (Show,Eq)

data Expression = 
    BinaryOperator Operator Expression Expression |
    Literal Val                                   |
    Function String [Expression]                  |
    Return Val 
    deriving (Show,Eq)

data Statement =
    SExpression Expression                                  |
    SCompound [Statement]                                   |

    SSelectionIf Condition Statement                        |
    SSelectionIfElse Condition Statement Else Statement     |

    SIterationWhile Condition Statement                     |
    SIterationDoWhile Statement Condition                   |
    SIterationFor Statement Condition Expression Statement  |

    SBreak                                                  |
    SContinue                                               |
    SReturn Expression                                      |
    SGoto Identifier                                        |

    SDeclaration
    deriving (Show,Eq)
    


data Operator = 
    Plus | 
    Minus |
    Times |
    Divide
    deriving (Show,Eq)
