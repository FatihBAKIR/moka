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

parse_typename :: [TokenType] -> (Expected TypeName ParseError, [TokenType])
parse_typename ((Id typename):
                (Single LeftBracket):
                (Literal (IntegerLiteral n)):
                (Single RightBracket):
                rest) =
  (Moka.Tokens.Just (ArrayN typename (read n :: Int)), rest)

parse_typename ((Id typename):rest) =
  (Moka.Tokens.Just (TypeN typename), rest)

parse_typename l = (Unexpected NoMatch, l)
