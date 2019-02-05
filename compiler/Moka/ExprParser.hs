module Moka.ExprParser where

import Moka
import Moka.Grammar
import Moka.Tokens
import Moka.ParserCommon

parse_funcall_expr    :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_literal_expr    :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_paren_expr      :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_hi_binary_expr  :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_lo_binary_expr  :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_unary_expr      :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_expr'           :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_expr            :: [TokenType] -> (Expected Expression ParseError, [TokenType])
parse_id_expr         :: [TokenType] -> (Expected Expression ParseError, [TokenType])

parse_id_expr ((Id id):rest) = (Moka.Just (Obj id), rest)
parse_id_expr l = parse_literal_expr l

parse_literal_expr ((Literal l_id):rest) = 
  (Moka.Just (Lit l_id), rest)

parse_literal_expr l = (Unexpected NoMatch, l)

parse_lo_binary_expr lst = help (parse_hi_binary_expr lst) where
  help (Moka.Just lexpr, (Single Plus):rest) = case (parse_lo_binary_expr rest) of
      (Moka.Just rexpr, rem) -> (Moka.Just (Bin (Single Plus) lexpr rexpr), rem)
      err -> err
  help (Moka.Just lexpr, (Single Minus):rest) = case (parse_lo_binary_expr rest) of
    (Moka.Just rexpr, rem) -> (Moka.Just (Bin (Single Minus) lexpr rexpr), rem)
    err -> err
  help el = el

parse_hi_binary_expr lst = help (parse_funcall_expr lst) where
  help (Moka.Just lexpr, (Single Star):rest) = case (parse_hi_binary_expr rest) of
      (Moka.Just rexpr, rem) -> (Moka.Just (Bin (Single Star) lexpr rexpr), rem)
      err -> err
  help (Moka.Just lexpr, (Single Slash):rest) = case (parse_hi_binary_expr rest) of
    (Moka.Just rexpr, rem) -> (Moka.Just (Bin (Single Slash) lexpr rexpr), rem)
    err -> err
  help el = el

parse_unary_expr ((Single Plus):rest) = help (parse_expr rest) where
  help (Moka.Just expr, rem) = (Moka.Just (Un (Single Plus) expr), rem)
  help err = err

parse_unary_expr ((Single Minus):rest) = help (parse_expr rest) where
  help (Moka.Just expr, rem) = (Moka.Just (Un (Single Minus) expr), rem)
  help err = err

parse_unary_expr l = parse_id_expr l

parse_arguments :: [TokenType] -> (Expected [Expression] ParseError, [TokenType])
parse_arguments ((Single RightParen):rem) = (Moka.Just [], rem)
parse_arguments l = case parse_expr l of
  (Unexpected err, _) -> (Unexpected err, l)
  (Moka.Just x, (Single RightParen):rem) -> (Moka.Just [x], rem)
  (Moka.Just x, (Single Comma):rem) -> case parse_arguments rem of
    (Unexpected err, _) -> (Unexpected err, l)
    (Moka.Just lst, rem) -> (Moka.Just $ x:lst, rem)

parse_funcall_expr l@((Id fname):(Single LeftParen):rest) = case parse_arguments rest of
  (Moka.Just x, rem) -> (Moka.Just $ FunCall fname x, rem)
  (Unexpected err, _) -> (Unexpected err, l)
parse_funcall_expr l = parse_paren_expr l

parse_paren_expr ((Single LeftParen):rest) = help (parse_expr rest) where
  help (Moka.Just expr, (Single RightParen):rem) = (Moka.Just (Paren expr), rem)
  help (Moka.Just expr, rem) = (Unexpected UnclosedParen, [])
  help _ = (Unexpected UnclosedParen, [])

parse_paren_expr lst = parse_unary_expr lst

parse_expr' [] = (Unexpected NoMatch, [])
parse_expr' lst = parse_lo_binary_expr lst

parse_expr l = help (parse_expr' l) where
  help (Moka.Just (Paren (Paren expr)), rst) = (Moka.Just (Paren expr), rst)
  help x = x
