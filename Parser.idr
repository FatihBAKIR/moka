module Lex

import Grammar
import Tokens

%access public export

data ParseError = UnclosedParen | NoMatch | Unknown | MissingSemicolon

parse_literal_expr : List TokenType -> (Expected Expression ParseError, List TokenType)
parse_paren_expr : List TokenType -> (Expected Expression ParseError, List TokenType)
parse_hi_binary_expr : List TokenType -> (Expected Expression ParseError, List TokenType)
parse_lo_binary_expr : List TokenType -> (Expected Expression ParseError, List TokenType)
parse_expr : List TokenType -> (Expected Expression ParseError, List TokenType)

parse_literal_expr ((Literal l_id)::rest) = 
  (Just (Lit l_id), rest)

parse_literal_expr l = (Unexpected NoMatch, l)

parse_lo_binary_expr lst with (parse_hi_binary_expr lst)
  | (Just lexpr, (Single Plus)::rest) = case (parse_lo_binary_expr rest) of
      (Just rexpr, rem) => (Just (Bin (Single Plus) lexpr rexpr), rem)
      err => err
  | (Just lexpr, (Single Minus)::rest) = case (parse_lo_binary_expr rest) of
    (Just rexpr, rem) => (Just (Bin (Single Minus) lexpr rexpr), rem)
    err => err
  | el = el

parse_hi_binary_expr lst with (parse_paren_expr lst)
  | (Just lexpr, (Single Star)::rest) = case (parse_hi_binary_expr rest) of
      (Just rexpr, rem) => (Just (Bin (Single Star) lexpr rexpr), rem)
      err => err
  | (Just lexpr, (Single Slash)::rest) = case (parse_hi_binary_expr rest) of
    (Just rexpr, rem) => (Just (Bin (Single Slash) lexpr rexpr), rem)
    err => err
  | el = el

parse_unary_expr : List TokenType -> (Expected Expression ParseError, List TokenType)
parse_unary_expr ((Single Plus)::rest) with (parse_expr rest)
  | (Just expr, rem) = (Just (Un (Single Plus) expr), rem)
  | err = err

parse_unary_expr ((Single Minus)::rest) with (parse_expr rest)
  | (Just expr, rem) = (Just (Un (Single Minus) expr), rem)
  | err = err

parse_unary_expr l = parse_literal_expr l

parse_paren_expr ((Single LeftParen)::rest) with (parse_expr rest)
  | (Just expr, (Single RightParen)::rem) = (Just (Paren expr), rem)
  | (Just expr, rem) = (Unexpected UnclosedParen, [])
  | _ = (Unexpected UnclosedParen, [])

parse_paren_expr lst = parse_unary_expr lst

parse_expr' : List TokenType -> (Expected Expression ParseError, List TokenType)
parse_expr' [] = (Unexpected NoMatch, [])
parse_expr' lst = parse_lo_binary_expr lst

parse_expr l with (parse_expr' l)
  | (Just (Paren (Paren expr)), rst) = (Just (Paren expr), rst)
  | x = x

parse_member_def : List TokenType -> (Expected DataMember ParseError, List TokenType)

parse_member_def ((Id typetok@(Identifier type_name))::
                  (Id nametok@(Identifier name))::
                  (Single At)::
                  (Literal l_id@(IntegerLiteral n))::
                  (Single SemiColon)::
                  rest) = 
  (Just (LayoutedMem (TypeN typetok) nametok (Layout l_id)), rest)

parse_member_def ((Id typetok@(Identifier type_name))::
                  (Id nametok@(Identifier name))::
                  (Single Assign)::
                  rest) = case parse_expr rest of
  (Just expr, (Single SemiColon)::rem) => (Just (InitMem (TypeN typetok) nametok expr), rem)
  (Just expr, rem) => (Unexpected MissingSemicolon, [])
  _ => (Unexpected Unknown, [])

parse_member_def ((Id typetok@(Identifier type_name))::
                  (Id nametok@(Identifier name))::
                  (Single SemiColon)::
                  rest) = 
  (Just (RawMem (TypeN typetok) nametok), rest)

parse_member_def lst = (Unexpected NoMatch, lst)
