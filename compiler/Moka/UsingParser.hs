module Moka.UsingParser where

import Moka.ExprParser
import Moka.Tokens
import Moka.Grammar
import Moka.ParserCommon

parse_using :: [TokenType] -> (Expected UsingDef ParseError, [TokenType])

parse_using ((Keyw Using):(Id typename):(Single Assign):rest) = 
  case parse_typename rest of
    (Moka.Tokens.Just x, ((Single SemiColon):rem)) -> (Moka.Tokens.Just (Alias typename x), rem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_using l = (Unexpected NoMatch, l)