module Lex

import Tokens
import Prelude.Show

%access public export

Show SingleCharTokens where
  show Plus     = "Plus"
  show Assign   = "Assign"
  show Slash     = "Slash"
  show Dot        = "Dot"
  show Star     = "Star"
  show Comma     = "Comma"
  show Colon     = "Colon"
  show Percent     = "Percent"
  show Quote     = "Quote"
  show DoubleQuote     = "DoubleQuote"
  show NewLine     = "NewLine"
  show Minus     = "Minus"
  show SemiColon     = "SemiColon"
  show LeftAngular     = "LeftAngular"
  show RightAngular     = "RightAngular"
  show Underscore     = "Underscore"
  show Whitespace     = "Whitespace"
  show LeftBrace     = "LeftBrace"
  show RightBrace     = "RightBrace"
  show (Digit x) = "Digit " ++ (show x)
  show (Alpha x) = "Alpha " ++ (show x)

Show DoubleCharTokens where
  show EqEq = "EqEq"
  show PlusAssign = "PlusAssign"
  show MinusAssign = "MinusAssign"
  show MulAssign = "MulAssign"
  show DivAssign = "DivAssign"
  show Decrement = "Decrement"
  show Increment = "Increment"
  show LessEq = "LessEq"
  show GreatEq = "GreatEq"

Show Literals where
  show (StringLiteral str) = "StringLiteral " ++ (show str)
  show (IntegerLiteral str) = "IntegerLiteral " ++ (show str)
  show (FloatLiteral str) = "FloatLiteral " ++ (show str)

Show NameTok where
  show (Identifier str) = "Identifier " ++ (show str)

Show Keywords where
  show Struct = "Struct"
  show Void = "Void"
  show From = "From"
  show Import = "Import"

Show TokenType where
  show (Keyw x) = "Keyw " ++ (show x)
  show (Single x) = "Single " ++ (show x)
  show (DoubleT x) = "DoubleT " ++ (show x)
  show (Literal x) = "Literal " ++ (show x)
  show (Id x) = "Id " ++ (show x)
  show NullTok = "NullTok"

Show Token where
  show (MkToken text tok) = "Token (" ++ (show tok) ++ ") " ++ (show text)
