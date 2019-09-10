module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Ast

binary s f assoc = Expr.Infix (Lexer.reserveOp s >> return (BinaryOperator f)) assoc

table = [[binary "*" Times Expr.AssocLeft,
          binary "/" Divide Expr.AssocLeft],
         [binary "+" Plus Expr.AssocLeft, 
          binary "-" Minus Expr.AssocLeft]]

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

declaration =  do
    reserve "int"
    id <- Lexer.ident
    semiend <- Lexer.semi
    return ([SQP PInt],
        [NPD $ DId id],[])

s_decl = do
    dec <- declaration
    return $ SDecl dec
    

statement :: Parser Statement
statement = try s_exp
    <|> try s_return
    <|> try s_comp
    <|> try s_decl
    
    

int :: Parser Expression
int = do
    n <- Lexer.int
    return $ Literal $ Val PInt $ show n

expression :: Parser Expression
expression =
    Expr.buildExpressionParser table factor

file :: Parser [Statement]
file =  many statement


factor :: Parser Expression
factor = try Parser.int


contents :: Parser a -> Parser a
contents p = do
    Token.whiteSpace lexer
    r <- p
    eof
    return r

parseAll :: String -> Either ParseError [Statement]
parseAll = parse (contents file) "<stdin>"
