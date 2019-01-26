module Main

import Lex
import Parser
import Tokens
import PrettyTokens
import Prelude.Show

extract_tok : Tok -> TokenType
extract_tok (Token _ tok) = tok

main : IO ()
main = do
  [prog, arg] <- getArgs
  file <- readFile arg
  case file of
    Right str => putStrLn (show (lex_many str))