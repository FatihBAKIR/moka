module Main where

import Moka
import Moka.Lex
import Moka.Tokens
import Moka.Grammar
import Moka.Parser
import System.Environment
import Moka.Codegen.CppGen

import Moka.Semantic.Types
import Moka.Semantic.Unit

extract_tok :: Tok -> TokenType
extract_tok (Token _ tok) = tok

map_toks = map extract_tok

get_ast (Moka.Just x, []) = x

lex_file :: String -> IO ([Tok])
lex_file path = do
  str <- readFile path
  pure(lex_many str)

main :: IO()
main = do
    [what, arg] <- getArgs
    tokens <- lex_file arg
    let toks = map_toks tokens
    let ast = get_ast $ parse_doc toks
    let syms = build_symtbl $ ast
    case what of
      "lex" -> putStrLn (show tokens)
      "lex1" -> putStrLn (show toks)
      "parse" -> putStrLn $ show ast
      "syms" -> do 
        putStrLn $ show $ to_list syms