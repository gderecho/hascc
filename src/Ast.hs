module Ast where

data Primitive =
    PInt          |
    PUInt         |
    PShort        |
    PUShort       |
    PLong         |
    PULong        |
    PLLong        |
    PULLong       |

    PChar         |
    PUChar        |
    PSChar        |

    PFloat        |
    PDouble       |
    PLDouble      
    
    deriving (Show,Eq)

data Val = Val Primitive String
    deriving (Show,Eq)

type Identifier = String

type Specifier = Primitive

data Expression = 
    BinaryOperator Operator Expression Expression |
    Literal Val                                   |
    Function String [Expression]                  |
    Return Val 
    deriving (Show,Eq)

type Condition = Expression

data Declarator =
    DPtr [Qualifier] Declarator |
    NPD NoPtrDeclarator
    deriving (Show,Eq)

type Param = (SpecQual,Identifier)

data NoPtrDeclarator =
    DId Identifier |
    DWrap Declarator | -- a declarator in parenthesis
    DArray    NoPtrDeclarator [Qualifier] Expression | -- returns void
    DArrayPtr NoPtrDeclarator [Qualifier] | 
    DFunction NoPtrDeclarator [Param]
    deriving (Show,Eq)

data Qualifier =
    QVoid          |
    QTyped String  |  -- a previous typedef result
    QStatic        |  
    QExtern        |
    QConst
    deriving (Show,Eq)

data SpecQual =
    SQP Primitive   |
    SQQ Qualifier   |
    SQTypedef |
    SQAuto |
    SQRegister
    deriving (Show,Eq)

    

type Initializer = Expression

type Declaration = ([SpecQual],[Declarator],[Initializer])

data Statement =
    SExpression Expression                                  |
    SCompound [Statement]                                   |

    SIf Condition Statement                        |
    SIfElse Condition Statement Statement          |
    SSwitch Condition Statement Statement          |

    SWhile Condition Statement                     |
    SDoWhile Statement Condition                   |
    SFor Statement Condition Expression Statement  |

    SBreak                                                  |
    SContinue                                               |
    SReturn Expression                                      |
    SGoto Identifier                                        | 
    SDecl Declaration
    deriving (Show,Eq)
    

{-
data LStatement =
    LStatement Identifier Statement           |
    LCase Identifier Statement                |
    LDefault Identifier Statement             
    deriving (Show,Eq)
-}
    
data Operator = 
    Plus  | 
    Minus |
    Times |
    Divide
    deriving (Show,Eq)
