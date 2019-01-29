module Lex

%access public export

data Expected t e = Just t | Unexpected e

data SingleCharTokens = Plus | Assign | Slash | Dot | Star | Comma 
                        | Colon | Percent | Quote | DoubleQuote | NewLine
                        | Minus | SemiColon | LeftAngular | RightAngular
                        | Digit Char | Alpha Char | Underscore | Whitespace
                        | LeftBrace | RightBrace | LeftParen | RightParen
                        | At

data DoubleCharTokens = EqEq | PlusAssign | 
                        MinusAssign | MulAssign | 
                        DivAssign | Decrement | 
                        Increment | LessEq | 
                        GreatEq | LambdaArrow

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
        
record Tok where
    constructor Token
    text : String
    tok : TokenType
