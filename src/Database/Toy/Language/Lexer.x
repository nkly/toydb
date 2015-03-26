{
module Database.Toy.Language.Lexer (Token(..), TokenClass(..), scanTokens) where
}

%wrapper "posn"

@num = [1-9]([0-9])*
@int = ("+" | "-")? @num
@double = @int "." [0-9]+

@name = [a-zA-Z0-9]+
@string = [^']*

@keywords = "create table" | "create index" | on | "drop table" | "drop index"
          | select | from | as | where | limit | "insert into"
          | values | update | set | "delete from" | vacuum

@types = int | double | varchar

@comparison = ">" | ">=" | "==" | "!=" | "<=" | "<"
@logical = and | or

@misc = ";" | "." | "," | "(" | ")" | "*" | "="

sql :-
    $white+             ;
    @keywords           { mkToken TKeyword }
    @types              { mkToken TType }
    @logical            { mkToken TLogicOp }
    @comparison         { mkToken TCompOp }
    @misc               { mkToken TMisc }
    @double             { mkToken TDouble }
    @int                { mkToken TInt }
    @name               { mkToken TName }
    ' @string '         { \p s -> mkToken TString p $ (init . tail) s }

{
data Token = Token Int TokenClass String
    deriving (Show)

data TokenClass = TInt | TDouble | TString | TName | TKeyword | TType | TCompOp | TLogicOp | TMisc
    deriving (Show)

mkToken :: TokenClass -> AlexPosn -> String -> Token
mkToken cls (AlexPn _ _ n) = Token n cls

scanTokens = alexScanTokens
}

