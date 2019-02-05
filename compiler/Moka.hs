module Moka where

data Expected t e = Just t | Unexpected e deriving Show
