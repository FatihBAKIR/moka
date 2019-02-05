module Moka.UnionParser where

import Moka
import Moka.ExprParser
import Moka.Tokens
import Moka.Grammar
import Moka.ParserCommon

parse_union_rhs :: [TokenType] -> (Expected [TypeName] ParseError, [TokenType])

parse_union_rhs lst = case parse_typename lst of
  (Unexpected err, _) -> (Unexpected err, lst)
  (Moka.Just x, (Single SemiColon):rest) -> (Moka.Just [x], rest)
  (Moka.Just x, (Single Bar):rest) -> case parse_union_rhs rest of
    (Moka.Just arr, rem) -> (Moka.Just (x:arr), rem)
    err -> err

parse_union :: [TokenType] -> (Expected UnionDef ParseError, [TokenType])

parse_union ((Keyw Union):(Id typename):(Single Assign):(Keyw Unsafe):rest) = 
  case parse_union_rhs rest of
    (Moka.Just x, rem) -> (Moka.Just (UnsafeUnion typename x), rem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_union ((Keyw Union):(Id typename):(Single Assign):rest) = 
  case parse_union_rhs rest of
    (Moka.Just x, rem) -> (Moka.Just (UnionT typename x), rem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_union l = (Unexpected NoMatch, l)