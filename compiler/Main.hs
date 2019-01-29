module Main where

import Moka
import Moka.Lex
import Moka.Tokens
import Moka.Grammar
import Moka.Parser
import System.Environment

extract_tok :: Tok -> TokenType
extract_tok (Token _ tok) = tok

map_toks = map extract_tok

lex_file :: String -> IO ([Tok])
lex_file path = do
  str <- readFile path
  pure(lex_many str)

main :: IO()
main = do
    [what, arg] <- getArgs
    tokens <- lex_file arg
    case what of
      "lex" -> putStrLn (show tokens)
      "lex1" -> putStrLn (show (map_toks tokens))
      "parse" -> case parse_doc (map_toks tokens) of
        (Moka.Tokens.Just x, []) -> putStrLn (show x)
        (Moka.Tokens.Just x, rest) -> putStrLn (show x)
        (Unexpected err, _) -> putStrLn (show err)