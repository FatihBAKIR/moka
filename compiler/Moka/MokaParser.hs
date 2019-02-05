module Moka.MokaParser where

import Moka
import Moka.ExprParser
import Moka.StructParser
import Moka.UnionParser
import Moka.UsingParser
import Moka.Grammar
import Moka.Tokens
import Moka.ParserCommon
import Moka.Parse.FuncParse

parse_doc :: [TokenType] -> (Expected Moka ParseError, [TokenType])
parse_doc [] = (Moka.Just (Doc []), [])

parse_doc x = case parse_struct_def x of
  (Moka.Just struct, rest) -> case parse_doc rest of
    (Moka.Just (Doc rem), []) -> (Moka.Just (Doc ((S struct):rem)), [])
    (Unexpected err, _) -> (Unexpected err, [])

  (Unexpected NoMatch, _) -> case parse_union x of
    (Moka.Just union, rest) -> case parse_doc rest of
      (Moka.Just (Doc rem), []) -> (Moka.Just (Doc ((U union):rem)), [])
      (Unexpected err, _) -> (Unexpected err, [])

    (Unexpected NoMatch, _) -> case parse_using x of
      (Unexpected NoMatch, _) -> case parse_expr_fun x of
        (Moka.Just fn, rest) -> case parse_doc rest of
          (Moka.Just (Doc rem), []) -> (Moka.Just (Doc ((F fn):rem)), [])
          (Unexpected err, _) -> (Unexpected err, [])
      (Moka.Just using, rest) -> case parse_doc rest of
        (Moka.Just (Doc rem), []) -> (Moka.Just (Doc ((A using):rem)), [])
        (Unexpected err, _) -> (Unexpected err, [])
      (Unexpected err, _) -> (Unexpected err, [])   
    (Unexpected err, _) -> (Unexpected err, [])

  (Unexpected err, _) -> (Unexpected err, [])