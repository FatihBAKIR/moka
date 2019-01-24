module Lex

import Prelude.Chars

data WhiteSpace = Tab | Space

data SingleCharTokens = Slash | Dot | Star | Comma | Colon | Percent | Quote | DoubleQuote | NewLine
data Literals = StringLiteral | IntegerLiteral | FloatLiteral

data NameTok = Identifier String

data TokenType = Single SingleCharTokens | Literal Literals | Id NameTok | NullTok

record Token where
    constructor MkToken
    text : String
    tok : TokenType

try_whitespace : Char -> Maybe WhiteSpace
try_whitespace ' ' = Just Space
try_whitespace '\t' = Just Tab
try_whitespace _ = Nothing

peek : String -> Char
peek str = strHead str

rest : String -> String
rest str = strTail str

legal_in_name : Char -> Bool
legal_in_name '_' = True
legal_in_name x = isAlphaNum x

try_tokenize_name : String -> Maybe NameTok
try_tokenize_name "" = Nothing
try_tokenize_name str with ((peek str), (rest str))
  | (c, rst) = case legal_in_name c of
    True => case try_tokenize_name rst of
      Just (Identifier str) => Just (Identifier (strCons c str))
      Nothing => Just (Identifier (singleton c))
    False => Nothing

lex_one' : List Char -> Token
lex_one' [] = MkToken "" NullTok
lex_one' (c::str) = case try_whitespace c of
  Just _ => lex_one' str
  Nothing => let string = singleton c
                 id = Id (Identifier string) in 
                  MkToken string id
