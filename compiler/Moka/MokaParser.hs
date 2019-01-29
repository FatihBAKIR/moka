module Moka.MokaParser where

import Moka.ExprParser
import Moka.StructParser
import Moka.UnionParser
import Moka.UsingParser
import Moka.Grammar
import Moka.Tokens
import Moka.ParserCommon

parse_doc :: [TokenType] -> (Expected Moka ParseError, [TokenType])
parse_doc [] = (Moka.Tokens.Just (Doc []), [])

parse_doc x = case parse_struct_def x of
  (Moka.Tokens.Just struct, rest) -> case parse_doc rest of
    (Moka.Tokens.Just (Doc rem), []) -> (Moka.Tokens.Just (Doc ((S struct):rem)), [])
    (Unexpected err, _) -> (Unexpected err, [])

  (Unexpected NoMatch, _) -> case parse_union x of
    (Moka.Tokens.Just union, rest) -> case parse_doc rest of
      (Moka.Tokens.Just (Doc rem), []) -> (Moka.Tokens.Just (Doc ((U union):rem)), [])
      (Unexpected err, _) -> (Unexpected err, [])
    (Unexpected NoMatch, _) -> case parse_using x of
      (Moka.Tokens.Just using, rest) -> case parse_doc rest of
        (Moka.Tokens.Just (Doc rem), []) -> (Moka.Tokens.Just (Doc ((A using):rem)), [])
        (Unexpected err, _) -> (Unexpected err, [])
      (Unexpected err, _) -> (Unexpected err, [])   
    (Unexpected err, _) -> (Unexpected err, [])

  (Unexpected err, _) -> (Unexpected err, [])