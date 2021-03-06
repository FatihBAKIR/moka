module Moka.Lex where

import Moka
import Moka.Tokens
import Data.Char

tokenize_one :: Char -> Maybe SingleCharTokens
tokenize_one '=' = Prelude.Just Assign

tokenize_one '<' = Prelude.Just LeftAngular
tokenize_one '>' = Prelude.Just RightAngular

tokenize_one '{' = Prelude.Just LeftBrace
tokenize_one '}' = Prelude.Just RightBrace

tokenize_one '(' = Prelude.Just LeftParen
tokenize_one ')' = Prelude.Just RightParen

tokenize_one '[' = Prelude.Just LeftBracket
tokenize_one ']' = Prelude.Just RightBracket

tokenize_one '+' = Prelude.Just Plus
tokenize_one '-' = Prelude.Just Minus
tokenize_one '*' = Prelude.Just Star
tokenize_one '/' = Prelude.Just Slash

tokenize_one '%' = Prelude.Just Percent

tokenize_one '.' = Prelude.Just Dot
tokenize_one ',' = Prelude.Just Comma
tokenize_one ':' = Prelude.Just Colon
tokenize_one ';' = Prelude.Just SemiColon

tokenize_one '@' = Prelude.Just At

tokenize_one '\'' = Prelude.Just Quote
tokenize_one '"' = Prelude.Just DoubleQuote

tokenize_one '_' = Prelude.Just Underscore

tokenize_one '\n' = Prelude.Just NewLine
tokenize_one '\t' = Prelude.Just Whitespace
tokenize_one ' ' = Prelude.Just Whitespace

tokenize_one '|' = Prelude.Just Bar
tokenize_one '&' = Prelude.Just Ampersand

tokenize_one x
  | isAlphaNum x = if isAlpha x then Prelude.Just (Alpha x) else Prelude.Just (Digit x)
  | otherwise = Nothing

tokenize_two :: SingleCharTokens -> SingleCharTokens -> Maybe DoubleCharTokens
tokenize_two Assign Assign = Prelude.Just EqEq

tokenize_two LeftAngular Assign = Prelude.Just LessEq
tokenize_two RightAngular Assign = Prelude.Just GreatEq

tokenize_two Plus Assign = Prelude.Just PlusAssign
tokenize_two Minus Assign = Prelude.Just MinusAssign
tokenize_two Star Assign = Prelude.Just MulAssign
tokenize_two Slash Assign = Prelude.Just DivAssign

tokenize_two Minus Minus = Prelude.Just Decrement
tokenize_two Plus Plus = Prelude.Just Increment

tokenize_two Assign RightAngular = Prelude.Just LambdaArrow

tokenize_two Slash Slash = Prelude.Just SlashSlash
tokenize_two Slash Star = Prelude.Just SlashStar
tokenize_two Star Slash = Prelude.Just StarSlash

tokenize_two _ _ = Nothing

is_newline :: Char -> Bool
is_newline '\n' = True
is_newline _ = False

legal_in_name :: Char -> Bool
legal_in_name '_' = True
legal_in_name x = isAlphaNum x

try_tokenize_name :: [Char] -> Maybe NameTok
try_tokenize_name "" = Nothing
try_tokenize_name (c:rst)
  | legal_in_name c = case try_tokenize_name rst of
    Prelude.Just (Identifier str) -> Prelude.Just (Identifier (c:str))
    Nothing -> Prelude.Just (Identifier [c])
  | otherwise = Nothing

data StringParseErrors = NewLineInString | UnterminatedString | Done deriving Show

try_tokenize_string :: String -> Bool -> Expected Literals StringParseErrors
try_tokenize_string "" _ = Unexpected Done

try_tokenize_string ('"':_) True = Moka.Just (StringLiteral "\"")

try_tokenize_string ('"':rest) False = case try_tokenize_string rest True of
  Moka.Just (StringLiteral res_str) -> Moka.Just (StringLiteral ('"':res_str))
  Unexpected err -> Unexpected err

try_tokenize_string ('\n':_) _ = Unexpected NewLineInString
try_tokenize_string (_:"") _ = Unexpected UnterminatedString

try_tokenize_string (c:rest) open = case try_tokenize_string rest open of
  Moka.Just (StringLiteral res_str) -> Moka.Just (StringLiteral (c:res_str))
  Unexpected err -> Unexpected err

legal_in_number :: SingleCharTokens -> Bool -> Bool
legal_in_number Dot _ = True
legal_in_number (Alpha 'a') True = True
legal_in_number (Alpha 'b') True = True
legal_in_number (Alpha 'c') True = True
legal_in_number (Alpha 'd') True = True
legal_in_number (Alpha 'e') True = True
legal_in_number (Alpha 'f') True = True
legal_in_number (Alpha 'A') True = True
legal_in_number (Alpha 'B') True = True
legal_in_number (Alpha 'C') True = True
legal_in_number (Alpha 'D') True = True
legal_in_number (Alpha 'E') True = True
legal_in_number (Alpha 'F') True = True
legal_in_number (Alpha 'x') False = True
legal_in_number (Alpha 'x') True = False
legal_in_number (Alpha 'X') False = True
legal_in_number (Alpha 'X') True = False
legal_in_number (Digit _) _ = True
legal_in_number _ _ = False

data NumParseErrors = MultipleDots | IllegalChar | Fin deriving Show

get_char :: SingleCharTokens -> Char
get_char (Digit c) = c
get_char (Alpha c) = c
get_char Dot = '.'
get_char _ = '?'

tokenize_number :: String -> Bool -> Bool -> Expected Literals NumParseErrors
tokenize_number "" _ _ = Unexpected Fin
tokenize_number (c:rst) has_dot is_hex = case tokenize_one c of
  Nothing -> Unexpected Fin
  Prelude.Just x -> case (x, rst) of
    (Dot, "") -> Unexpected IllegalChar

    (Whitespace, _) -> Unexpected Fin
    (Comma, _) -> Unexpected Fin
    (SemiColon, _) -> Unexpected Fin
    (Colon, _) -> Unexpected Fin
    (RightParen, _) -> Unexpected Fin
    (RightBracket, _) -> Unexpected Fin
    (RightBrace, _) -> Unexpected Fin
    (Plus, _) -> Unexpected Fin
    (Minus, _) -> Unexpected Fin
    (Star, _) -> Unexpected Fin
    (Slash, _) -> Unexpected Fin
    
    (Dot, rst) -> case has_dot of
      True -> Unexpected MultipleDots
      False -> case tokenize_number rst True is_hex of
        Unexpected Fin -> Unexpected Fin
        Unexpected x -> Unexpected x
        Moka.Just (IntegerLiteral str) -> Moka.Just (FloatLiteral ('.':str))

    (tok, rst) -> case legal_in_number tok is_hex of
      False -> Unexpected IllegalChar
      True -> case tokenize_number rst has_dot is_hex of
        Moka.Just (IntegerLiteral str) -> Moka.Just (IntegerLiteral ((get_char tok):str))
        Moka.Just (FloatLiteral str) -> Moka.Just (FloatLiteral ((get_char tok):str))
        Unexpected Fin -> Moka.Just (IntegerLiteral [get_char tok])
        Unexpected x -> Unexpected x

try_tokenize_number :: String -> Expected Literals NumParseErrors
try_tokenize_number x = tokenize_number x False False

peek :: [a] -> Maybe a
peek [] = Nothing
peek all@(x:(rst)) = Prelude.Just x

lex_one' :: [Char] -> (Tok, Int)
lex_one' [] = (Token "" NullTok, 0)
lex_one' all@(c:(str)) = case tokenize_one c of
  Prelude.Just NewLine -> case lex_one' str of
    (Token ret_str (Single NewLine), n) -> (Token ('\n':ret_str) (Single NewLine), n + 1)
    _ -> (Token "\n" (Single NewLine), 1)

  Prelude.Just Whitespace -> case lex_one' str of
    (tok, n) -> (tok, n + 1)

  Prelude.Just (Alpha c) -> case try_tokenize_name all of
    Prelude.Just s@(Identifier str) -> (Token str (Id s), length str)
  
  Prelude.Just (Digit c) -> case try_tokenize_number all of 
    Moka.Just s@(IntegerLiteral str) -> (Token str (Literal s), length str)
    Moka.Just s@(FloatLiteral str) -> (Token str (Literal s), length str)
    Unexpected err -> (Token "" NullTok, 0)

  Prelude.Just SemiColon -> (Token ";" (Single SemiColon), 1)

  Prelude.Just DoubleQuote -> case try_tokenize_string all False of
    Moka.Just s@(StringLiteral str) -> (Token str (Literal s), length str)
    Unexpected err -> (Token "" NullTok, 0)

  Prelude.Just tok -> case peek str of
    Nothing -> (Token [c] (Single tok), 1)
    Prelude.Just next_c -> case tokenize_one next_c of
      Nothing -> (Token [c] (Single tok), 1)
      Prelude.Just next_tok -> case tokenize_two tok next_tok of
        Nothing -> (Token [c] (Single tok), 1)
        Prelude.Just res -> (Token (c:(next_c:"")) (DoubleT res), 2)
  
  _ -> (Token "" NullTok, 0)

try_keywords :: NameTok -> Maybe Keywords
try_keywords (Identifier "struct") = Prelude.Just Struct
try_keywords (Identifier "void") = Prelude.Just Void
try_keywords (Identifier "import") = Prelude.Just Import
try_keywords (Identifier "from") = Prelude.Just From
try_keywords (Identifier "extern") = Prelude.Just Extern
try_keywords (Identifier "union") = Prelude.Just Union
try_keywords (Identifier "unsafe") = Prelude.Just Unsafe
try_keywords (Identifier "using") = Prelude.Just Using
try_keywords (Identifier "var") = Prelude.Just Var
try_keywords (Identifier "const") = Prelude.Just Const
try_keywords (Identifier "if") = Prelude.Just If
try_keywords (Identifier "else") = Prelude.Just Else
try_keywords (Identifier "for") = Prelude.Just For
try_keywords (Identifier "auto") = Prelude.Just Auto
try_keywords _ = Nothing

lex_one :: String -> (Tok, Int)
lex_one x = case lex_one' x of
    tk@(Token res_str (Id name), n) -> case try_keywords name of
      Nothing -> tk
      Prelude.Just keyw -> (Token res_str (Keyw keyw), n)
    tok -> tok

prog :: String -> Int -> String
prog str 0 = str
prog (_:str) n = prog str (n - 1)

lex_many' :: String -> [Tok]
lex_many' str = case lex_one str of
  (_, 0) -> []
  (Token _ NullTok, n) -> lex_many' (prog str n)
  (tok, n) -> tok : (lex_many' (prog str n))

is_newline' :: Tok -> Bool
is_newline' (Token _ (Single NewLine)) = True
is_newline' _ = False

find_slice :: [Tok] -> (Tok -> Bool) -> [Tok]
find_slice (x:rst) fun = case fun x of
  True -> (x:rst)
  False -> find_slice rst fun
find_slice [] _ = []

rm_comments :: [Tok] -> [Tok]
rm_comments ((Token _ (DoubleT SlashSlash)):toks) = case find_slice toks is_newline' of
  ((Token _ (Single NewLine)):rst) -> rm_comments rst
  _ -> []
rm_comments (x:rst) = (x:(rm_comments rst))
rm_comments [] = []

rm_newlines :: [Tok] -> [Tok]
rm_newlines toks = [tok | tok <- toks, not (is_newline' tok)]

lex_many :: String -> [Tok]
lex_many str = let orig = lex_many' str in
  rm_newlines (rm_comments orig) 