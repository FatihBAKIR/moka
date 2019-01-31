module Moka.Semantic.Types where

import Moka.Grammar
import Moka.Tokens

import Moka.Semantic.Unit

data TypeInfo = BuiltIn String |
                UserType TypeName
                deriving Show

class DeclType x where
    decltype :: SymTable -> x -> TypeInfo

instance DeclType Literals where
    decltype _ (IntegerLiteral _) = BuiltIn "int"
    decltype _ (FloatLiteral _) = BuiltIn "float"
    decltype _ (StringLiteral _) = BuiltIn "string"

bin_decltype :: TokenType -> TypeInfo -> TypeInfo -> TypeInfo

bin_decltype _ a@(BuiltIn "float") (BuiltIn "int") = a
bin_decltype _ (BuiltIn "int") a@(BuiltIn "float") = a

-- builtins don't change type with any operator yet
bin_decltype _ a@(BuiltIn x) (BuiltIn y)
  | x == y = a

fun_decltype :: SymTable -> FuncDef -> TypeInfo
fun_decltype _ (Function _ tp _ _) = UserType tp
fun_decltype x (ShortFun _ _ expr) = decltype x expr

instance DeclType Expression where
    decltype x (Lit l) = decltype x l
    decltype x (Un op expr) = decltype x expr
    decltype x (Bin op expr1 expr2) = bin_decltype op (decltype x expr1) (decltype x expr2)
    decltype x (Paren expr) = decltype x expr
    decltype x (FunCall (Identifier name) args) = case get_sym x name of
        Prelude.Just (F f) -> fun_decltype x f