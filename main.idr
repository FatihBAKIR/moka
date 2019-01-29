module Main

import Lex
import Parser
import Tokens
import PrettyTokens
import Prelude.Show
import Grammar

extract_tok : Tok -> TokenType
extract_tok (Token _ tok) = tok

parse_file : String -> IO (Expected StructDef ParseError)
parse_file path = do
  file <- readFile path
  case file of
    Right str => case parse_struct_def (map extract_tok (lex_many str)) of
      (x, _) => pure(x)
    Left _ => pure(Unexpected Unknown)

lex_file : String -> IO (List Tok)
lex_file path = do
  file <- readFile path
  case file of
    Right str => pure(lex_many str)
    Left _ => pure([])

main : IO ()
main = do
  [prog, what, arg] <- getArgs
  case what of
    "lex" => do
      tokens <- lex_file arg
      putStrLn (show tokens)
    "parse" => do
      def <- parse_file arg
      case def of
        Just x => putStrLn "success!"
        Unexpected err => putStrLn "error!"