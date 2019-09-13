module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Char as Char
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Ast

binary s f assoc = Expr.Infix (Lexer.reserveOp s >> return (BinaryOperator f)) assoc

table = [[binary "*" Times Expr.AssocLeft,
          binary "/" Divide Expr.AssocLeft],
         [binary "+" Plus Expr.AssocLeft, 
          binary "-" Minus Expr.AssocLeft]]

primitive :: Parser Primitive
primitive = 
    try ( 
        do 
            reserve "short"
            return PShort )
    <|>
    try (
        do
            reserve "int"
            return PInt )
    <|>
    try (
        do
            reserve "long"
            return PLong )
    <|>
    try ( 
        do
            reserve "char"
            return PChar )
    <|>
    try ( 
        do
            reserve "double"
            return PDouble )
    <|>
    try ( 
        do
            reserve "float"
            return PFloat )

qualifier :: Parser Qualifier
qualifier =
    try ( 
        do 
            reserve "void"
            return QVoid )
    <|>
    try (
        do
            reserve "static"
            return QStatic )
    <|>
    try (
        do
            reserve "extern"
            return QExtern )
    <|>
    try ( 
        do
            reserve "const"
            return QConst )

spec_qual :: Parser SpecQual
spec_qual =
    try (
        do 
            qual <- qualifier
            return $ SQQ qual)
    <|>
    try (
        do 
            prim <- primitive
            return $ SQP prim)
    <|>
    try (
        do 
            reserve "typedef"
            return SQTypedef)
    <|>
    try (
        do
            reserve "auto"
            return SQAuto)
    <|>
    try (
        do
            reserve "register"
            return SQRegister)

s_exp :: Parser Statement
s_exp = do
    exp <- Parser.expression
    semiend <- Lexer.semi
    return $ SExpression exp

s_comp :: Parser Statement
s_comp = do
    exps <- braces $ many statement
    return $ SCompound exps

s_return :: Parser Statement
s_return = do
    reserve "return"
    value <- Parser.expression
    semiend <- Lexer.semi
    return $ SReturn value

param :: Parser Param
param = do
    sq <- spec_qual
    id <- ident
    return (sq,id)

fn :: Parser Declarator
fn = do
    decl <- no_ptr_declarator
    open <- Char.char '(' 
    close <- Char.char ')' 
    return $ NPD $
        DFunction decl []

s_fn_definition :: Parser Statement
s_fn_definition = do
    sq <- many spec_qual
    f <- fn
    stmt <- statement
    return $ SFnDefinition (sq,[f],[]) stmt

no_ptr_declarator :: Parser NoPtrDeclarator
no_ptr_declarator =
    try (
        do 
            id <- ident
            return $ DId id)
    <|> try (
        do 
            decl <- parens declarator
            return $ DWrap decl)

declarator :: Parser Declarator
declarator =
    try fn
    <|> try (
        do
            npd <- no_ptr_declarator
            return $ NPD npd )
    <|> try (
        do
            ignore <- spaces
            star <- Char.char '*' 
            ignore <- spaces
            quals <- many qualifier
            decl <- declarator
            return $ DPtr [] decl ) 


declaration =  do
    sq <- many spec_qual
    id <- Lexer.commasep declarator
    semiend <- Lexer.semi
    return (sq,id,[])

s_decl = do
    dec <- declaration
    return $ SDecl dec

s_if = do
    reserve "if"
    cond <- parens condition
    body <- statement
    return $ SIf cond body

s_while = do
    reserve "while"
    cond <- parens condition
    body <- statement
    return $ SWhile cond body

s_break = do
    reserve "break"
    semiend <- Lexer.semi
    return SBreak;
    

statement :: Parser Statement
statement = try s_exp
    <|> try s_return
    <|> try s_comp
    <|> try s_decl
    <|> try s_if
    <|> try s_while
    <|> try s_break
    <|> try s_fn_definition
    
    

int :: Parser Expression
int = do
    n <- Lexer.int
    return $ Literal $ Val PInt $ show n

double :: Parser Expression
double = do
    n <- Lexer.double
    return $ Literal $ Val PDouble $ show n

expression :: Parser Expression
expression =
    Expr.buildExpressionParser table factor

condition :: Parser Condition
condition = expression


file :: Parser [Statement]
file =  many statement


factor :: Parser Expression
factor = try Parser.double
    <|> try Parser.int


contents :: Parser a -> Parser a
contents p = do
    Token.whiteSpace lexer
    r <- p
    eof
    return r

parseAll :: String -> Either ParseError [Statement]
parseAll = parse (contents file) "<stdin>"
