module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where 
        style = emptyDef {
            Token.commentStart = "/*",
            Token.commentEnd = "*/",
            Token.commentLine = "//",
            Token.nestedComments = True,
            Token.reservedOpNames = ["+","-","*","/",";"],
            Token.reservedNames = ["int","return"],
            Token.caseSensitive = True
        }

int :: Parser Integer
int = Token.natural lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semicolon :: Parser a -> Parser [a]
semicolon = Token.semiSep lexer

