module Main where

import Moka
import Moka.Lex
import Moka.Tokens
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
    [arg] <- getArgs
    tokens <- lex_file arg
    putStrLn (show (map_toks tokens))