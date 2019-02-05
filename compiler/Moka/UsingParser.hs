module Moka.UsingParser where

import Moka
import Moka.ExprParser
import Moka.Tokens
import Moka.Grammar
import Moka.ParserCommon

parse_using :: [TokenType] -> (Expected UsingDef ParseError, [TokenType])

parse_using ((Keyw Using):(Id typename):(Single Assign):rest) = 
  case parse_typename rest of
    (Moka.Just x, ((Single SemiColon):rem)) -> (Moka.Just (Alias typename x), rem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_using l = (Unexpected NoMatch, l)