module Lex

import Grammar
import Tokens

%access public export

parse_expr : List TokenType -> (Maybe Expression, List TokenType)

parse_literal_expr : List TokenType -> (Maybe Literals, List TokenType)

parse_literal_expr ((Literal l_id)::rest) = 
  (Just l_id, rest)

parse_literal_expr l = (Nothing, l)

--to_operator : TokenType -> Operators
--to_operator x@(Single Plus) = Op x
--to_operator (Single Minus) = Low Sub
--to_operator (Single Star) = High Multiply
--to_operator (Single Slash) = High Divide

parse_unary_expr : List TokenType -> (Maybe UnaryExpr, List TokenType)
parse_unary_expr ((Single Plus)::
                  rest) with (parse_expr rest)
  | (Just expr, rem) = (Just (Unary (Op (Single Plus)) expr), rem)
  | (Nothing, _) = (Nothing, rest)

parse_expr all = (Nothing, all)

parse_member_def : List TokenType -> Maybe DataMember

parse_member_def ((Id typetok@(Identifier type_name))::
                  (Id nametok@(Identifier name))::
                  (Single At)::
                  (Literal l_id@(IntegerLiteral n))::
                  (Single SemiColon)::
                  rest) = 
  Just (LayoutedMem (TypeN typetok) nametok (Layout l_id))

parse_member_def ((Id typetok@(Identifier type_name))::
                  (Id nametok@(Identifier name))::
                  (Single Assign)::
                  (Literal l_id@(IntegerLiteral n))::
                  (Single SemiColon)::
                  rest) = 
  Just (InitMem (TypeN typetok) nametok (Lit l_id))

parse_member_def ((Id typetok@(Identifier type_name))::
                  (Id nametok@(Identifier name))::
                  (Single SemiColon)::
                  rest) = 
  Just (RawMem (TypeN typetok) nametok)

parse_member_def _ = Nothing
