{-# LANGUAGE OverloadedStrings #-}

module Moka.Codegen.CppGen where

import Moka.Grammar
import Moka.Tokens
import Data.Aeson

import Text.Megaparsec
import Text.Mustache

obj = object
  [ "name"   .= ("John" :: String)
  , "things" .= ["pen" :: String, "candle", "egg"]
  ]

template :: Template
template = case compileMustacheText "" "Hi, {{name}}! You have:\n{{#things}}\n  * {{.}}\n{{/things}}\n" of
  Right t -> t

use = renderMustache template obj

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

make_names :: Int -> [String]
make_names 0 = []
make_names x = (show (length rest)):rest where rest = make_names (x - 1)

generate_union :: UnionDef -> String
generate_union (UnionT name elems) =
    "struct " ++ (generate_name name) ++ "{\n" ++
        "int type_;" ++
        "union {" ++
            generate_members [(RawMem tp (Identifier nm)) | tp <- elems
                                                          , nm <- (make_names (length elems))] ++
        "}" ++
    "}"