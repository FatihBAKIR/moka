module Moka.Grammar where

import Moka.Tokens

data TypeName = TypeN NameTok deriving Show

data LayoutId = Layout Literals deriving Show

data Expression = Lit Literals | 
                  Bin TokenType Expression Expression |
                  Un TokenType Expression |
                  Paren Expression |
                  Empty 
                  deriving Show

data DataMember = RawMem TypeName NameTok | 
                  LayoutedMem TypeName NameTok LayoutId |
                  InitMem TypeName NameTok Expression
                  deriving Show

data StructDef = Structure NameTok [DataMember] deriving Show
