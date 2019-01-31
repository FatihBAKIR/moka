module Moka.Grammar where

import Moka.Tokens

data TypeName = TypeN NameTok | 
                ArrayN TypeName Int 
                deriving Show

data LayoutId = Layout Literals 
                deriving Show

data Expression = FunCall NameTok [Expression] |
                  Obj NameTok |
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

data Statement =  Expr Expression deriving Show

data Param = NamedParam TypeName NameTok | UnnamedParam TypeName deriving Show

data FuncDef =  Function NameTok TypeName [Param] [Statement] | 
                ShortFun NameTok [Param] Expression
                deriving Show

data StructDef = Structure NameTok [DataMember] deriving Show
data UnionDef = UnionT NameTok [TypeName] | UnsafeUnion NameTok [TypeName] deriving Show
data UsingDef = Alias NameTok TypeName deriving Show

data Definiton = F FuncDef | U UnionDef | S StructDef | A UsingDef | Extern Definiton deriving Show

data Moka = Doc [Definiton] deriving Show