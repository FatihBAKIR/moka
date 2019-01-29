module Moka.Tokens where

data Expected t e = Just t | Unexpected e deriving Show

data SingleCharTokens = Plus | Assign | Slash | Dot | Star | Comma 
                        | Colon | Percent | Quote | DoubleQuote | NewLine
                        | Minus | SemiColon | LeftAngular | RightAngular
                        | Digit Char | Alpha Char | Underscore | Whitespace
                        | LeftBrace | RightBrace | LeftParen | RightParen
                        | At
                        deriving Show

data DoubleCharTokens = EqEq | PlusAssign | 
                        MinusAssign | MulAssign | 
                        DivAssign | Decrement | 
                        Increment | LessEq | 
                        GreatEq | LambdaArrow
                        deriving Show

data Literals = StringLiteral String | 
                IntegerLiteral String | 
                FloatLiteral String
                deriving Show

data NameTok =  Identifier String
                deriving Show

data Keywords = Struct | Void | Import | From
                deriving Show

data TokenType =  Keyw Keywords | 
                  Single SingleCharTokens | 
                  DoubleT DoubleCharTokens | 
                  Literal Literals | 
                  Id NameTok | 
                  NullTok
                  deriving Show
        
data Tok = Token String TokenType deriving Show