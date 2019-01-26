module Lex

import Tokens

%access public export

data TypeName = TypeN NameTok

data LayoutId = Layout Literals

data Operators = Op TokenType

data Expression : Type

data BinaryExpr = Binary Expression Operators Expression
data UnaryExpr = Unary Operators Expression

data Expression = Lit Literals | 
                  Bin BinaryExpr |
                  Un UnaryExpr |
                  Paren Expression

data DataMember = RawMem TypeName NameTok | 
                  LayoutedMem TypeName NameTok LayoutId |
                  InitMem TypeName NameTok Expression

data StructDef = Structure NameTok (List DataMember)