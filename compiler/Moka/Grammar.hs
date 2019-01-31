module Moka.Grammar where

import Moka.Tokens

data TypeName = TypeN NameTok | 
                ArrayN TypeName Int 
                deriving Show

data LayoutId = Layout Literals 
                deriving Show

data Expression = Fun NameTok [Expression] |
                  Lit Literals | 
                  Bin TokenType Expression Expression |
                  Un TokenType Expression |
                  Paren Expression |
                  Empty 
                  deriving Show

data DataMember = RawMem TypeName NameTok | 
                  LayoutedMem TypeName NameTok LayoutId |
                  InitMem TypeName NameTok Expression
                  deriving Show

data Comment = Line String | Block String

data StructDef = Structure NameTok [DataMember] deriving Show
data UnionDef = UnionT NameTok [TypeName] | UnsafeUnion NameTok [TypeName] deriving Show
data UsingDef = Alias NameTok TypeName deriving Show

data TypeDef = U UnionDef | S StructDef | A UsingDef | Extern TypeDef deriving Show

data Moka = Doc [TypeDef] deriving Show