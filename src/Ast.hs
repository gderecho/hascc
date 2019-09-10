module Ast where

data Primitive =
    PInteger
    deriving (Show,Eq)

data Val = Val Primitive String
    deriving (Show,Eq)

data Identifier = Identifier String
    deriving (Show,Eq)

data Specifier =
    SInt
    deriving (Show, Eq)

data Expression = 
    BinaryOperator Operator Expression Expression |
    Literal Val                                   |
    Function String [Expression]                  |
    Return Val 
    deriving (Show,Eq)

type Condition = Expression

data Declarator =
    DFunction Identifier -- returns void
    deriving (Show,Eq)

data Declaration = 
    DeclarationOnly Specifier Declarator 
    -- |
    -- DeclarationInit Specifier Declarator Initializer
    deriving (Show,Eq)

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

    SDeclaration Declaration Statement
    deriving (Show,Eq)
    

data LStatement =
    LStatement Identifier Statement           |
    LCase Identifier Statement                |
    LDefault Identifier Statement             
    deriving (Show,Eq)
    

data Operator = 
    Plus  | 
    Minus |
    Times |
    Divide
    deriving (Show,Eq)
