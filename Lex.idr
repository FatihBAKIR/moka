module Lex

import Prelude.Chars

%access public export

data WhiteSpace = Tab | Space

data SingleCharTokens = Slash | Dot | Star | Comma | Colon | Percent | Quote | DoubleQuote | NewLine
data Literals = StringLiteral String | IntegerLiteral String | FloatLiteral String

data NameTok = Identifier String

data TokenType = Single SingleCharTokens | Literal Literals | Id NameTok | NullTok

data MyStream = XStream String Nat

data Expected t e = Just t | Unexpected e

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

legal_in_number : Char -> Bool -> Bool
legal_in_number '.' _ = True
legal_in_number 'a' True = True
legal_in_number 'b' True = True
legal_in_number 'c' True = True
legal_in_number 'd' True = True
legal_in_number 'e' True = True
legal_in_number 'f' True = True
legal_in_number 'A' True = True
legal_in_number 'B' True = True
legal_in_number 'C' True = True
legal_in_number 'D' True = True
legal_in_number 'E' True = True
legal_in_number 'F' True = True
legal_in_number 'x' False = True
legal_in_number 'x' True = False
legal_in_number 'X' False = True
legal_in_number 'X' True = False
legal_in_number x _ = case isAlphaNum x of
  False => False
  True => not (isAlpha x)

data NumParseErrors = MultipleDots | IllegalChar | Fin

tokenize_number : String -> Bool -> Bool -> Expected Literals NumParseErrors
tokenize_number str has_dot is_hex with ((strHead str), (rest str))
  | ('-', rst) = case tokenize_number rst has_dot is_hex of
    Unexpected Fin => Unexpected Fin
    Unexpected x => Unexpected x
    Just (IntegerLiteral str) => Just (IntegerLiteral (strCons '-' str))
  | (' ', _) = Unexpected Fin
  | (',', _) = Unexpected Fin
  | (';', _) = Unexpected Fin
  | ('.', "") = Unexpected IllegalChar
  | (c, "") = case legal_in_number c is_hex of
    True => Just (IntegerLiteral (singleton c))
    False => Unexpected IllegalChar
  | ('.', rst) = case has_dot of
    True => Unexpected MultipleDots
    False => case tokenize_number rst True is_hex of
      Unexpected Fin => Unexpected Fin
      Unexpected x => Unexpected x
      Just (IntegerLiteral str) => Just (IntegerLiteral (strCons '.' str))
  | (c, rst) = case legal_in_number c is_hex of
    True => case tokenize_number rst has_dot is_hex of
      Just (IntegerLiteral str) => Just (IntegerLiteral (strCons c str))
      Unexpected Fin => Just (IntegerLiteral (singleton c))
      Unexpected x => Unexpected x
    False => Unexpected IllegalChar

try_tokenize_number : String -> Expected Literals NumParseErrors
try_tokenize_number x = tokenize_number x False False

lex_one' : List Char -> (Token, Nat)
lex_one' [] = (MkToken "" NullTok, 0)
lex_one' all@(c::str) = case try_whitespace c of
  Just _ => case lex_one' str of
    (tok, n) => (tok, n + 1)
  Nothing => case isAlpha c of
    True => case try_tokenize_name (pack all) of
      Just s@(Identifier str) => (MkToken str (Id s), length str)
      Nothing => (MkToken "" NullTok, 0)
    False => case (c == '-') || ((isAlphaNum c) && not (isAlpha c)) of
      True => case try_tokenize_number (pack all) of 
        Just s@(IntegerLiteral str) => (MkToken str (Literal s), length str)
        Unexpected err => (MkToken "" NullTok, 0)
      False => (MkToken "" NullTok, 0)

lex_one : String -> (Token, Nat)
lex_one str with (unpack str)
  | x = lex_one' x


prog : String -> Nat -> String
prog str n = substr n 25 str

lex_many : String -> List Token
lex_many str with (lex_one str)
  | (_, Z) = []
  | (tok, n) = tok :: (lex_many (prog str n))