module Moka.Codegen.CppGen where

import Moka.Grammar
import Moka.Tokens

add_newline :: String -> String
add_newline x = x ++ "\n"

generate_name :: NameTok -> String
generate_name (Identifier n) = n

generate_typename :: TypeName -> String
generate_typename (TypeN tok) = generate_name tok
generate_typename (ArrayN name extent) = "std::array<" ++ (generate_typename name) ++ ", " ++ (show extent) ++ ">"

generate_alias :: UsingDef -> String
generate_alias (Alias name other) = 
    "using " ++ (generate_typename (TypeN name)) ++ " = " ++ (generate_typename other) ++ ";"

generate_member :: DataMember -> String

generate_member (RawMem tp name) =
    (generate_typename tp) ++ " " ++ (generate_name name) ++ ";"

generate_members :: [DataMember] -> String
generate_members lst = foldl (++) "" (map add_newline (map generate_member lst))

generate_struct :: StructDef -> String
generate_struct (Structure name mems) =
    "struct " ++ (generate_name name) ++ "{\n" ++ (generate_members mems) ++ "}"
