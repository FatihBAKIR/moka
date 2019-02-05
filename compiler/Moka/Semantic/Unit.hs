module Moka.Semantic.Unit where

import Moka.Grammar
import Moka.Tokens

import System.IO.Unsafe

import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

type SymTable = HashTable String Definition

get_def_name :: Definition -> String
get_def_name (U (UnionT (Identifier name) _)) = name
get_def_name (U (UnsafeUnion (Identifier name) _)) = name
get_def_name (S (Structure (Identifier name) _)) = name
get_def_name (A (Alias (Identifier name) _)) = name
get_def_name (F (ShortFun (Identifier name) _ _ _)) = name

get_sym :: SymTable -> String -> Maybe Definition
get_sym tbl name = unsafePerformIO $ do
  res <- H.lookup tbl name
  pure(res)

to_list :: SymTable -> [(String, Definition)]
to_list tbl = unsafePerformIO $ unsafePerformIO $ do
  pure(H.toList tbl)

build_symtbl :: Moka -> SymTable
build_symtbl (Doc l) = unsafePerformIO $ do
  st <- H.new
  let add x = H.insert st (get_def_name x) x in mapM_ add l
  pure(st)