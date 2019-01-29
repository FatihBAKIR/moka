module Moka.StructParser where

import Moka.Tokens
import Moka.Grammar
import Moka.ExprParser

parse_member_def  :: [TokenType] -> (Expected DataMember    ParseError, [TokenType])
parse_member_defs :: [TokenType] -> (Expected [DataMember]  ParseError, [TokenType])
parse_struct_def  :: [TokenType] -> (Expected StructDef     ParseError, [TokenType])

parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  (Single At):
                  (Literal l_id@(IntegerLiteral n)):
                  (Single SemiColon):
                  rest) = 
  (Moka.Tokens.Just (LayoutedMem (TypeN typetok) nametok (Layout l_id)), rest)

parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  (Single At):
                  (Literal l_id@(IntegerLiteral n)):rest) = (Unexpected MissingSemicolon, [])

parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  (Single At):rest) = (Unexpected MissingLayoutId, [])

parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  (Single Assign):(Single SemiColon):rest) = (Unexpected MissingInitializer, [])

parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  (Single Assign):
                  rest) = case parse_expr rest of
  (Moka.Tokens.Just expr, (Single SemiColon):rem) -> (Moka.Tokens.Just (InitMem (TypeN typetok) nametok expr), rem)
  (Moka.Tokens.Just expr, rem) -> (Unexpected MissingSemicolon, [])
  (Unexpected err, rem) -> (Unexpected err, [])
  
parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  (Single SemiColon):
                  rest) = 
  (Moka.Tokens.Just (RawMem (TypeN typetok) nametok), rest)

parse_member_def ((Id typetok@(Identifier type_name)):
                  (Id nametok@(Identifier name)):
                  rest) = (Unexpected MissingSemicolon, [])

parse_member_def lst = (Unexpected NoMatch, lst)

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

parse_struct_def _ = (Unexpected MissingStruct, [])