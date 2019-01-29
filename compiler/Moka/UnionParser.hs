module Moka.UnionParser where

import Moka.ExprParser
import Moka.Tokens
import Moka.Grammar
import Moka.ParserCommon

parse_union_rhs :: [TokenType] -> (Expected [TypeName] ParseError, [TokenType])

parse_union_rhs lst = case parse_typename lst of
  (Unexpected err, _) -> (Unexpected err, lst)
  (Moka.Tokens.Just x, (Single SemiColon):rest) -> (Moka.Tokens.Just [x], rest)
  (Moka.Tokens.Just x, (Single Bar):rest) -> case parse_union_rhs rest of
    (Moka.Tokens.Just arr, rem) -> (Moka.Tokens.Just (x:arr), rem)
    err -> err

parse_union :: [TokenType] -> (Expected UnionDef ParseError, [TokenType])

parse_union ((Keyw Union):(Id typename):(Single Assign):(Keyw Unsafe):rest) = 
  case parse_union_rhs rest of
    (Moka.Tokens.Just x, rem) -> (Moka.Tokens.Just (UnsafeUnion typename x), rem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_union ((Keyw Union):(Id typename):(Single Assign):rest) = 
  case parse_union_rhs rest of
    (Moka.Tokens.Just x, rem) -> (Moka.Tokens.Just (UnionT typename x), rem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_union l = (Unexpected NoMatch, l)