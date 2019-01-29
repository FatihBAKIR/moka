module Moka.ParserCommon where
    
import Moka.Tokens
import Moka.Grammar

data ParseError = UnclosedParen | 
                  NoMatch | 
                  Unknown | 
                  MissingSemicolon | 
                  MissingStruct |
                  UnclosedBrace |
                  MissingLayoutId |
                  MissingInitializer
                  deriving Show

parse_array :: TypeName -> [TokenType] -> (Expected TypeName ParseError, [TokenType])

parse_array base ((Single LeftBracket):
                  (Literal (IntegerLiteral n)):
                  (Single RightBracket):rem) =
  parse_array (ArrayN base (read n :: Int)) rem

parse_array base rem = (Moka.Tokens.Just base, rem) 

parse_typename :: [TokenType] -> (Expected TypeName ParseError, [TokenType])

parse_typename ((Id typename):
                all@((Single LeftBracket):
                (Literal (IntegerLiteral n)):
                (Single RightBracket):
                rest)) =
  parse_array (TypeN typename) all

parse_typename ((Id typename):rest) =
  (Moka.Tokens.Just (TypeN typename), rest)

parse_typename l = (Unexpected NoMatch, l)
