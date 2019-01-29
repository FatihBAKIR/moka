module Lex

import Tokens

%access public export

data TypeName = TypeN NameTok

data LayoutId = Layout Literals

data Expression : Type

data Expression = Lit Literals | 
                  Bin TokenType Expression Expression |
                  Un TokenType Expression |
                  Paren Expression |
                  Empty

data DataMember = RawMem TypeName NameTok | 
                  LayoutedMem TypeName NameTok LayoutId |
                  InitMem TypeName NameTok Expression

data StructDef = Structure NameTok (List DataMember)