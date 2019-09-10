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


int :: Parser Expression
int = do
    n <- Lexer.int
    return $ Literal $ Val PInteger $ show n

expression :: Parser Expression
expression =
    Expr.buildExpressionParser table factor

function :: Parser Expression
function = do
    reserve "int"
    name   <- ident
    params <- parens $ many expression
    body   <- braces $ semicolon expression
    return $ Function name $ body


ret :: Parser Expression
ret = do
    reserve "return"
    value <- Parser.int
    return $ Return $ Val PInteger $ show value


factor :: Parser Expression
factor = try Parser.int
    <|> try ret
    <|> try function
    <|> braces expression
    <|> parens expression

contents :: Parser a -> Parser a
contents p = do
    Token.whiteSpace lexer
    r <- p
    eof
    return r

parseAll :: String -> Either ParseError Expression
parseAll = parse (contents expression) "<stdin>"


