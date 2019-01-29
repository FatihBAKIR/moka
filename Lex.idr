module Lex

import Prelude.Chars
import Tokens
import PrettyTokens

%access public export

get_char : SingleCharTokens -> Char
get_char (Digit c) = c
get_char (Alpha c) = c
get_char Dot = '.'
get_char _ = '?'

tokenize_one : Char -> Maybe SingleCharTokens
tokenize_one '=' = Just Assign

tokenize_one '<' = Just LeftAngular
tokenize_one '>' = Just RightAngular

tokenize_one '{' = Just LeftBrace
tokenize_one '}' = Just RightBrace

tokenize_one '(' = Just LeftParen
tokenize_one ')' = Just RightParen

tokenize_one '+' = Just Plus
tokenize_one '-' = Just Minus
tokenize_one '*' = Just Star
tokenize_one '/' = Just Slash

tokenize_one '%' = Just Percent

tokenize_one '.' = Just Dot
tokenize_one ',' = Just Comma
tokenize_one ':' = Just Colon
tokenize_one ';' = Just SemiColon

tokenize_one '@' = Just At

tokenize_one '\'' = Just Quote
tokenize_one '"' = Just DoubleQuote

tokenize_one '_' = Just Underscore

tokenize_one '\n' = Just NewLine
tokenize_one '\t' = Just Whitespace
tokenize_one ' ' = Just Whitespace

tokenize_one x with (isAlphaNum x)
  | True = if isAlpha x then Just (Alpha x) else Just (Digit x)
  | False = Nothing

tokenize_two : SingleCharTokens -> SingleCharTokens -> Maybe DoubleCharTokens
tokenize_two Assign Assign = Just EqEq

tokenize_two LeftAngular Assign = Just LessEq
tokenize_two RightAngular Assign = Just GreatEq

tokenize_two Plus Assign = Just PlusAssign
tokenize_two Minus Assign = Just MinusAssign
tokenize_two Star Assign = Just MulAssign
tokenize_two Slash Assign = Just DivAssign

tokenize_two Minus Minus = Just Decrement
tokenize_two Plus Plus = Just Increment

tokenize_two Assign RightAngular = Just LambdaArrow

tokenize_two _ _ = Nothing

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

data StringParseErrors = NewLineInString | UnterminatedString | Done

try_tokenize_string : String -> Bool -> Expected Literals StringParseErrors
try_tokenize_string "" _ = Unexpected Done
try_tokenize_string str is_open with (strHead str, strTail str, is_open)
  | ('"', _, True) = Just (StringLiteral "\"")
  | ('"', rest, False) = case try_tokenize_string rest True of
    Just (StringLiteral res_str) => Just (StringLiteral (strCons '"' res_str))
    Unexpected err => Unexpected err
  | ('\n', _, _) = Unexpected NewLineInString
  | (_, "", _) = Unexpected UnterminatedString
  | (c, rest, open) = case try_tokenize_string rest open of
    Just (StringLiteral res_str) => Just (StringLiteral (strCons c res_str))
    Unexpected err => Unexpected err
  
legal_in_number : SingleCharTokens -> Bool -> Bool
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

data NumParseErrors = MultipleDots | IllegalChar | Fin

tokenize_number : String -> Bool -> Bool -> Expected Literals NumParseErrors
tokenize_number "" _ _ = Unexpected Fin
tokenize_number str has_dot is_hex with (tokenize_one (strHead str))
  | Nothing = Unexpected Fin
  | Just x = case (x, rest str) of
    (Dot, "") => Unexpected IllegalChar

    (Whitespace, _) => Unexpected Fin
    (Comma, _) => Unexpected Fin
    (SemiColon, _) => Unexpected Fin
    (Colon, _) => Unexpected Fin
    (RightParen, _) => Unexpected Fin
    (Plus, _) => Unexpected Fin
    (Minus, _) => Unexpected Fin
    (Star, _) => Unexpected Fin
    (Slash, _) => Unexpected Fin
    
    (Dot, rst) => case has_dot of
      True => Unexpected MultipleDots
      False => case tokenize_number rst True is_hex of
        Unexpected Fin => Unexpected Fin
        Unexpected x => Unexpected x
        Just (IntegerLiteral str) => Just (FloatLiteral (strCons '.' str))

    (tok, rst) => case legal_in_number tok is_hex of
      False => Unexpected IllegalChar
      True => case tokenize_number rst has_dot is_hex of
        Just (IntegerLiteral str) => Just (IntegerLiteral (strCons (get_char tok) str))
        Just (FloatLiteral str) => Just (FloatLiteral (strCons (get_char tok) str))
        Unexpected Fin => Just (IntegerLiteral (singleton (get_char tok)))
        Unexpected x => Unexpected x

try_tokenize_number : String -> Expected Literals NumParseErrors
try_tokenize_number x = tokenize_number x False False

peek : List a -> Maybe a
peek [] = Nothing
peek all@(x::(rst)) = Just x

lex_one' : List Char -> (Tok, Nat)
lex_one' [] = (Token "" NullTok, 0)
lex_one' all@(c::(str)) = case tokenize_one c of
  Just NewLine => case lex_one' str of
    (Token ret_str (Single NewLine), n) => (Token (strCons '\n' ret_str) (Single NewLine), n + 1)
    _ => (Token "\n" (Single NewLine), 1)

  Just Whitespace => case lex_one' str of
    (tok, n) => (tok, n + 1)

  Just (Alpha c) => case try_tokenize_name (pack all) of
    Just s@(Identifier str) => (Token str (Id s), length str)
  
  Just (Digit c) => case try_tokenize_number (pack all) of 
    Just s@(IntegerLiteral str) => (Token str (Literal s), length str)
    Just s@(FloatLiteral str) => (Token str (Literal s), length str)
    Unexpected err => (Token "" NullTok, 0)

  Just SemiColon => (Token ";" (Single SemiColon), 1)

  Just DoubleQuote => case try_tokenize_string (pack all) False of
    Just s@(StringLiteral str) => (Token str (Literal s), length str)
    Unexpected err => (Token "" NullTok, 0)

  Just tok => case peek str of
    Nothing => (Token (singleton c) (Single tok), 1)
    Just next_c => case tokenize_one next_c of
      Nothing => (Token (singleton c) (Single tok), 1)
      Just next_tok => case tokenize_two tok next_tok of
        Nothing => (Token (singleton c) (Single tok), 1)
        Just res => (Token (strCons c (strCons next_c "")) (DoubleT res), 2)
  
  _ => (Token "" NullTok, 0)

try_keywords : NameTok -> Maybe Keywords
try_keywords (Identifier "struct") = Just Struct
try_keywords (Identifier "void") = Just Void
try_keywords (Identifier "import") = Just Import
try_keywords (Identifier "from") = Just From
try_keywords _ = Nothing

lex_one : String -> (Tok, Nat)
lex_one str with (unpack str)
  | x = case lex_one' x of
    tk@(Token res_str (Id name), n) => case try_keywords name of
      Nothing => tk
      Just keyw => (Token res_str (Keyw keyw), n)
    tok => tok

prog : String -> Integer -> String
prog str 0 = str
prog str n = prog (strTail str) (n - 1)

lex_many : String -> List Tok
lex_many str with (lex_one str)
  | (_, Z) = []
  | (Token _ (Single NewLine), n) = lex_many (prog str (cast n))
  | (Token _ NullTok, n) = lex_many (prog str (cast n))
  | (tok, n) = tok :: (lex_many (prog str (cast n)))
