module Moka.StructParser where

import Moka.ExprParser
import Moka.Tokens
import Moka.Grammar
import Moka.ParserCommon

parse_member_def' :: TypeName -> [TokenType] -> (Expected DataMember    ParseError, [TokenType])
parse_member_def  :: [TokenType] -> (Expected DataMember    ParseError, [TokenType])
parse_member_defs :: [TokenType] -> (Expected [DataMember]  ParseError, [TokenType])
parse_struct_def  :: [TokenType] -> (Expected StructDef     ParseError, [TokenType])

parse_member_def lst = case parse_typename lst of
  (Moka.Tokens.Just tp, rest) -> parse_member_def' tp rest
  (Unexpected err, _) -> (Unexpected err, [])

parse_member_def' tp ((Id nametok):
                  (Single At):
                  (Literal l_id@(IntegerLiteral n)):
                  (Single SemiColon):
                  rest) = 
  (Moka.Tokens.Just (LayoutedMem tp nametok (Layout l_id)), rest)

parse_member_def' _ ((Id nametok):
                  (Single At):
                  (Literal l_id@(IntegerLiteral n)):
                  rest) = (Unexpected MissingSemicolon, [])

parse_member_def' _ ((Id nametok):
                  (Single At):rest) = (Unexpected MissingLayoutId, [])

parse_member_def' _ ((Id nametok):
                  (Single Assign):
                  (Single SemiColon):
                  rest) = (Unexpected MissingInitializer, [])

parse_member_def' tp ((Id nametok):
                  (Single Assign):
                  rest) = case parse_expr rest of
  (Moka.Tokens.Just expr, (Single SemiColon):rem) -> (Moka.Tokens.Just (InitMem tp nametok expr), rem)
  (Moka.Tokens.Just expr, rem) -> (Unexpected MissingSemicolon, [])
  (Unexpected err, rem) -> (Unexpected err, [])
  
parse_member_def' tp ((Id nametok):
                  (Single SemiColon):
                  rest) = 
  (Moka.Tokens.Just (RawMem tp nametok), rest)

parse_member_def' _ ((Id nametok):
                  rest) = (Unexpected MissingSemicolon, [])

parse_member_def' tp lst = (Unexpected NoMatch, lst)

parse_member_defs [] = (Unexpected UnclosedBrace, [])
parse_member_defs ((Single RightBrace):rem) = (Moka.Tokens.Just [], (Single RightBrace):rem)

parse_member_defs lst = help (parse_member_def lst) where
  help (Unexpected err, _) = (Unexpected err, [])
  help (Moka.Tokens.Just def, rem) = case parse_member_defs rem of
    (Moka.Tokens.Just defs, rrem) -> (Moka.Tokens.Just (def:defs), rrem)
    (Unexpected err, _) -> (Unexpected err, [])

parse_struct_def ((Keyw Struct):
                  (Id nametok@(Identifier type_name)):
                  (Single LeftBrace):
                  rest) = case parse_member_defs rest of
  (Moka.Tokens.Just defs, (Single RightBrace):rem) -> (Moka.Tokens.Just (Structure nametok defs), rem)
  (Moka.Tokens.Just defs, rem) -> (Unexpected UnclosedBrace, [])
  (Unexpected err, _) -> (Unexpected err, [])

parse_struct_def _ = (Unexpected NoMatch, [])