module Main

import Lex
import PrettyTokens
import Prelude.Show

main : IO ()
main = do
  [prog, arg] <- getArgs
  file <- readFile arg
  case file of
    Right str => putStrLn (show (lex_many str))