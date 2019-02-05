module Moka.Parse.FuncParse where

import Moka
import Moka.ExprParser
import Moka.Tokens
import Moka.Grammar
import Moka.ParserCommon

parse_expr_fun  :: [TokenType] -> (Expected FuncDef ParseError, [TokenType])

parse_fun_params ::  [TokenType] -> (Expected [Param] ParseError, [TokenType])
parse_fun_params ((Single RightParen):rem) = (Moka.Just [], rem)

parse_fun_params lst = case parse_typename lst of
  (Moka.Just Infer, rem) -> (Unexpected CantInferParam, lst)
  (Moka.Just typename, (Single Comma):rst) -> let par = (UnnamedParam typename) in
    case parse_fun_params rst of
      (Moka.Just l, rem) -> (Moka.Just (par:l), rem)
      err -> err
  (Moka.Just typename, (Id name):(Single Comma):rst) -> let par = (NamedParam typename name) in
    case parse_fun_params rst of
      (Moka.Just l, rem) -> (Moka.Just (par:l), rem)
      err -> err
  (Moka.Just typename, (Id name):(Single RightParen):rst) -> let par = (NamedParam typename name) in
    (Moka.Just [par], rst)
  (Moka.Just typename, (Single RightParen):rem) -> let par = (UnnamedParam typename) in
    (Moka.Just [par], rem)
  (Unexpected err, _) -> (Unexpected err, lst)

parse_expr_fun' ty ((Id name_fun):(Single LeftParen):lst) = case parse_fun_params lst of
  (Unexpected err, _) -> (Unexpected err, lst)
  (Moka.Just params, rem) -> case rem of
    ((DoubleT LambdaArrow):rst) -> case parse_expr rst of
      (Moka.Just expr, r) -> (Moka.Just (ShortFun name_fun ty params expr), r)
      (Unexpected err, _) -> (Unexpected err, rst)

parse_expr_fun lst = case parse_typename lst of
  (Moka.Just ty, rem) -> case parse_expr_fun' ty rem of
    (Moka.Just fn, r) -> (Moka.Just fn, r)
