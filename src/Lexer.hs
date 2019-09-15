module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

rnames :: [String]
rnames = [
        "auto",
        "break",
        "case",
        "char",
        "const",
        "continue",
        "default",
        "do",
        "double",
        "else",
        "enum",
        "extern",
        "float",
        "for",
        "goto",
        "if",
        "int",
        "long",
        "register",
        "return",
        "short",
        "signed",
        "sizeof",
        "static",
        "struct",
        "switch",
        "typedef",
        "union",
        "unsigned",
        "void",
        "volatile",
        "while"
    ]

rops = [
    "=",
    "+=",
    "-=",
    "*=",
    "/=",
    "=",
    "%=",
    "&=",
    "|=",
    "^=",
    "<<=",
    ">>=",

    "++",
    "--",

    "+",
    "-",
    "*",
    "/",
    "%",
    "&",
    "|",
    "^",
    "<<",
    ">>",

    "!",
    "&&",
    "||",

    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">=",

    "->",
    ".",
    "->*",
    ".*",

    ",",
    "?",
    ":"
    
    ]

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where 
        style = emptyDef {
            Token.commentStart = "/*",
            Token.commentEnd = "*/",
            Token.commentLine = "//",
            Token.nestedComments = True,
            Token.reservedOpNames = rops,
            Token.reservedNames = rnames,
            Token.caseSensitive = True
        }

int :: Parser Integer
int = Token.natural lexer

double :: Parser Double
double = Token.float lexer

char :: Parser Char
char = Token.charLiteral lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semi :: Parser String
semi = Token.semi lexer

cchar_p :: Parser String
cchar_p = Token.stringLiteral lexer

commasep :: Parser a -> Parser [a]
commasep = Token.commaSep1 lexer

commasep_or_none :: Parser a -> Parser [a]
commasep_or_none = Token.commaSep lexer

ident :: Parser String
ident = Token.identifier lexer

reserve :: String -> Parser ()
reserve = Token.reserved lexer



reserveOp :: String -> Parser ()
reserveOp = Token.reservedOp lexer

