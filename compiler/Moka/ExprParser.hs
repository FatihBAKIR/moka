module Moka.ExprParser where

import Moka.Grammar
import Moka.Tokens
import Moka.ParserCommon

--parse_funcall_expr    :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_literal_expr    :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_paren_expr      :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_hi_binary_expr  :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_lo_binary_expr  :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_unary_expr      :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_expr'           :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_expr            :: [TokenType] -> (Expected Expression ParseError, [TokenType])

parse_literal_expr ((Literal l_id):rest) = 
  (Moka.Tokens.Just (Lit l_id), rest)

parse_literal_expr l = (Unexpected NoMatch, l)

parse_lo_binary_expr lst = help (parse_hi_binary_expr lst) where
  help (Moka.Tokens.Just lexpr, (Single Plus):rest) = case (parse_lo_binary_expr rest) of
      (Moka.Tokens.Just rexpr, rem) -> (Moka.Tokens.Just (Bin (Single Plus) lexpr rexpr), rem)
      err -> err
  help (Moka.Tokens.Just lexpr, (Single Minus):rest) = case (parse_lo_binary_expr rest) of
    (Moka.Tokens.Just rexpr, rem) -> (Moka.Tokens.Just (Bin (Single Minus) lexpr rexpr), rem)
    err -> err
  help el = el

parse_hi_binary_expr lst = help (parse_paren_expr lst) where
  help (Moka.Tokens.Just lexpr, (Single Star):rest) = case (parse_hi_binary_expr rest) of
      (Moka.Tokens.Just rexpr, rem) -> (Moka.Tokens.Just (Bin (Single Star) lexpr rexpr), rem)
      err -> err
  help (Moka.Tokens.Just lexpr, (Single Slash):rest) = case (parse_hi_binary_expr rest) of
    (Moka.Tokens.Just rexpr, rem) -> (Moka.Tokens.Just (Bin (Single Slash) lexpr rexpr), rem)
    err -> err
  help el = el

parse_unary_expr ((Single Plus):rest) = help (parse_expr rest) where
  help (Moka.Tokens.Just expr, rem) = (Moka.Tokens.Just (Un (Single Plus) expr), rem)
  help err = err

parse_unary_expr ((Single Minus):rest) = help (parse_expr rest) where
  help (Moka.Tokens.Just expr, rem) = (Moka.Tokens.Just (Un (Single Minus) expr), rem)
  help err = err

parse_unary_expr l = parse_literal_expr l

parse_paren_expr ((Single LeftParen):rest) = help (parse_expr rest) where
  help (Moka.Tokens.Just expr, (Single RightParen):rem) = (Moka.Tokens.Just (Paren expr), rem)
  help (Moka.Tokens.Just expr, rem) = (Unexpected UnclosedParen, [])
  help _ = (Unexpected UnclosedParen, [])

parse_paren_expr lst = parse_unary_expr lst

parse_expr' [] = (Unexpected NoMatch, [])
parse_expr' lst = parse_lo_binary_expr lst

parse_expr l = help (parse_expr' l) where
  help (Moka.Tokens.Just (Paren (Paren expr)), rst) = (Moka.Tokens.Just (Paren expr), rst)
  help x = x
