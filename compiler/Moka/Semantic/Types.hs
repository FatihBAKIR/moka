module Moka.Semantic.Types where

import Moka.Grammar
import Moka.Tokens

data TypeInfo = BuiltIn String
                deriving Show

class DeclType x where
    decltype :: x -> TypeInfo

instance DeclType Literals where
    decltype (IntegerLiteral _) = BuiltIn "int"
    decltype (FloatLiteral _) = BuiltIn "float"
    decltype (StringLiteral _) = BuiltIn "string"

bin_decltype :: TokenType -> TypeInfo -> TypeInfo -> TypeInfo

bin_decltype _ a@(BuiltIn "float") (BuiltIn "int") = a
bin_decltype _ (BuiltIn "int") a@(BuiltIn "float") = a

-- builtins don't change type with any operator yet
bin_decltype _ a@(BuiltIn x) (BuiltIn y)
  | x == y = a

instance DeclType Expression where
    decltype (Lit l) = decltype l
    decltype (Un op expr) = decltype expr
    decltype (Bin op expr1 expr2) = bin_decltype op (decltype expr1) (decltype expr2)
    decltype (Paren expr) = decltype expr