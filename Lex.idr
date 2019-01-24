module Lex

import Prelude.Chars

data WhiteSpace = Tab | Space

data SingleCharTokens = Slash | Dot | Star | Comma | Colon | Percent | Quote | DoubleQuote | NewLine
data Literals = StringLiteral | IntegerLiteral | FloatLiteral

data NameTok = Identifier String

data TokenType = Single SingleCharTokens | Literal Literals | Id NameTok | NullTok

data MyStream = XStream String Nat

make_stream : String -> MyStream
make_stream str = XStream str 0

peek : MyStream -> Char
peek (XStream str idx) = strIndex str (cast idx + 1)

consume_one : MyStream -> (Char, MyStream)
consume_one (XStream str idx) = (strHead str, XStream str (idx + 1))

record Token where
    constructor MkToken
    text : String
    tok : TokenType

try_whitespace : Char -> Maybe WhiteSpace
try_whitespace ' ' = Just Space
try_whitespace '\t' = Just Tab
try_whitespace _ = Nothing

is_newline : Char -> Bool
is_newline '\n' = True
is_newline _ = False

rest : String -> String
rest str = strTail str

legal_in_name : Char -> Bool
legal_in_name '_' = True
legal_in_name x = isAlphaNum x

try_tokenize_name : String -> Maybe NameTok
try_tokenize_name "" = Nothing
try_tokenize_name str with ((strHead str), (rest str))
  | (c, rst) = case legal_in_name c of
    True => case try_tokenize_name rst of
      Just (Identifier str) => Just (Identifier (strCons c str))
      Nothing => Just (Identifier (singleton c))
    False => Nothing

lex_one' : List Char -> (Token, Nat)
lex_one' [] = (MkToken "" NullTok, 0)
lex_one' all@(c::str) = case try_whitespace c of
  Just _ => case lex_one' str of
    (tok, n) => (tok, n + 1)
  Nothing => case legal_in_name c of
    True => case try_tokenize_name (pack all) of
      Just s@(Identifier str) => (MkToken "" (Id s), length str)
      Nothing => (MkToken "" NullTok, 0)
    False => (MkToken "" NullTok, 0)

lex_one : String -> (Token, Nat)
lex_one str with (unpack str)
  | x = lex_one' x