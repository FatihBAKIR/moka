module Main

import Lex
import Parser
import Tokens
import PrettyTokens
import Grammar

extract_tok : Tok -> TokenType
extract_tok (Token _ tok) = tok

map_toks : List Tok -> List TokenType
map_toks = map extract_tok

lex_file : String -> IO (List Tok)
lex_file path = do
  file <- readFile path
  case file of
    Right str => pure(lex_many str)
    Left _ => pure([])

do_parse : List Tok -> String
do_parse tokens = case parse_struct_def (map_toks tokens) of
  (Just (Structure (Identifier name) mems), _) => name ++ " " ++ (show (length mems))
  _ => "can't parse"

main : IO ()
main = do
  [prog, what, arg] <- getArgs
  tokens <- lex_file arg
  case what of
    "lex" => do
      putStrLn (show tokens)
    "parse" => putStrLn (do_parse tokens)