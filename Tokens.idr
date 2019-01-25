module Lex

%access public export

data SingleCharTokens = Plus | Assign | Slash | Dot | Star | Comma 
                        | Colon | Percent | Quote | DoubleQuote | NewLine
                        | Minus | SemiColon | LeftAngular | RightAngular
                        | Digit Char | Alpha Char | Underscore | Whitespace
                        | LeftBrace | RightBrace

data DoubleCharTokens = EqEq | PlusAssign | 
                        MinusAssign | MulAssign | 
                        DivAssign | Decrement | 
                        Increment | LessEq | 
                        GreatEq

data Literals = StringLiteral String | 
                IntegerLiteral String | 
                FloatLiteral String

data NameTok = Identifier String

data Keywords = Struct | Void | Import | From

data TokenType =  Keyw Keywords | 
                  Single SingleCharTokens | 
                  DoubleT DoubleCharTokens | 
                  Literal Literals | 
                  Id NameTok | 
                  NullTok
        
record Token where
    constructor MkToken
    text : String
    tok : TokenType