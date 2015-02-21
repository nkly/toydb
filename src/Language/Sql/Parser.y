{
module Language.Sql.Parser where

import Language.Sql.Lexer
import Language.Sql.Command
}

%name       command
%tokentype  { Token }
%error      { parseError }

%token
    INT                 { Token _ TInt $$ }
    DOUBLE              { Token _ TDouble $$ }
    STRING              { Token _ TString $$ }
    NAME                { Token _ TName $$ }

    'create table'      { Token _ TKeyword "create table" }
    'create index'      { Token _ TKeyword "create index" }
    'on'                { Token _ TKeyword "on" }
    'drop table'        { Token _ TKeyword "drop table" }
    'drop index'        { Token _ TKeyword "drop index" }
    'select'            { Token _ TKeyword "select" }
    'from'              { Token _ TKeyword "from" }
    'as'                { Token _ TKeyword "as" }
    'where'             { Token _ TKeyword "where" }
    'limit'             { Token _ TKeyword "limit" }
    'insert into'       { Token _ TKeyword "insert into" }
    'values'            { Token _ TKeyword "values" }
    'update'            { Token _ TKeyword "update" }
    'set'               { Token _ TKeyword "set" }
    'delete from'       { Token _ TKeyword "delete from" }
    'vacuum'            { Token _ TKeyword "vacuum" }

    'int'               { Token _ TType "int" }
    'double'            { Token _ TType "double" }
    'varchar'           { Token _ TType "varchar" }

    '>'                 { Token _ TCompOp ">" }
    '>='                { Token _ TCompOp ">=" }
    '=='                { Token _ TCompOp "==" }
    '!='                { Token _ TCompOp "!=" }
    '<='                { Token _ TCompOp "<=" }
    '<'                 { Token _ TCompOp "<" }

    'and'               { Token _ TLogicOp "and" }
    'or'                { Token _ TLogicOp "or" }

    ';'                 { Token _ TMisc ";" }
    '.'                 { Token _ TMisc "." }
    ','                 { Token _ TMisc "," }
    '('                 { Token _ TMisc "(" }
    ')'                 { Token _ TMisc ")" }
    '*'                 { Token _ TMisc "*" }
    '='                 { Token _ TMisc "=" }

%%

Command : Command1 ';'  { $1 }

Command1 : 'create table' NAME '(' ColumnDefs ')'
            { CreateTable $2 $4 }
         | 'create index' NAME 'on' NAME '(' ColumnNames ')'
            { CreateIndex $2 $4 $6 }
         | 'drop table' NAME
            { DropTable $2 }
         | 'drop index' NAME
            { DropIndex $2 }
         | 'select' Projection 'from' Tables MaybeWhereClause MaybeLimit
            { Select $2 $4 $5 $6 }
         | 'insert into' NAME 'values' '(' InsertValues ')'
            { Insert $2 $5 }
         | 'update' NAME 'set' '(' UpdateValues ')' MaybeWhereClause
            { Update $2 $5 $7 }
         | 'delete from' NAME MaybeWhereClause
            { Delete $2 $3 }
         | 'vacuum'
            { Vacuum }

ColumnDefs : ColumnDef                      { [$1] }
           | ColumnDefs ',' ColumnDef       { $1 ++ [$3] }

ColumnDef : NAME 'int'                  { ($1, ColInt) }
          | NAME 'double'               { ($1, ColDouble) }
          | NAME 'varchar' '(' INT ')'  { ($1, ColVarchar (read $4)) }

ColumnNames : NAME                  { [$1] }
            | ColumnNames ',' NAME  { $1 ++ [$3] }

Projection : '*'                    { All }
           | Selectors              { Some $1 }

Selectors : Selector                { [$1] }
          | Selectors ',' Selector  { $1 ++ [$3] }

Selector : NAME                     { OneColumn (Column $1) $1 }
         | NAME 'as' NAME           { OneColumn (Column $1) $3 }
         | NAME '.' NAME            { OneColumn (QualColumn $1 $3) ($1 ++ "." ++ $3) }
         | NAME '.' NAME 'as' NAME  { OneColumn (QualColumn $1 $3) $5 }
         | NAME '.' '*'             { WholeTable $1 }

Tables : NAME               { [$1] }
       | Tables ',' NAME    { $1 ++ [$3] }

MaybeWhereClause : 'where' WhereClause  { Just $2 }
                 | {- empty -}          { Nothing }

WhereClause : WhereClause 'or' WhereClause1     { Or $1 $3 }
            | WhereClause1                      { $1 }

WhereClause1 : WhereClause1 'and' WhereTerm     { And $1 $3 }
             | WhereTerm                        { $1 }

WhereTerm : WhereSelector '>' WhereSelector     { Gt $1 $3 }
          | WhereSelector '>=' WhereSelector    { Ge $1 $3 }
          | WhereSelector '==' WhereSelector    { Eq $1 $3 }
          | WhereSelector '!=' WhereSelector    { Ne $1 $3 }
          | WhereSelector '<=' WhereSelector    { Le $1 $3 }
          | WhereSelector '<' WhereSelector     { Lt $1 $3 }
          | '(' WhereClause ')'                 { $2 }

WhereSelector : NAME            { WSColumn (Column $1) }
              | NAME '.' NAME   { WSColumn (QualColumn $1 $3) }
              | Value           { WSValue $1 }

MaybeLimit : 'limit' INT    { Just (read $2 :: Int) }
           | {- empty -}    { Nothing }

InsertValues : Value                    { [$1] }
             | InsertValues ',' Value   { $1 ++ [$3] }

UpdateValues : NAME '=' Value                   { [($1, $3)] }
             | UpdateValues ',' NAME '=' Value  { $1 ++ [($3, $5)] }

Value : INT     { VInt (read $1) }
      | DOUBLE  { VDouble (read $1) }
      | STRING  { VString $1 }

{
parseError :: [Token] -> a
parseError t = error $ show t

parseCommand :: String -> Command
parseCommand = command . scanTokens
}
