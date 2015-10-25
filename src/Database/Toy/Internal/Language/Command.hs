{-|
    This module contains types that describe SQL statement to be executed by
    DBMS.
-}

module Database.Toy.Internal.Language.Command where

import Database.Toy.Internal.Prelude

type TableName  = String
type ColumnName = String
type IndexName  = String
type Alias      = String

data Command = CreateTable TableName [(ColumnName, ColumnType)]
             | CreateIndex IndexName TableName [ColumnName]
             | DropTable TableName
             | DropIndex IndexName
             | Select Projection [TableName] (Maybe WhereClause) (Maybe Int)
             | Insert TableName [Value]
             | Update TableName [(ColumnName, Value)] (Maybe WhereClause)
             | Delete TableName (Maybe WhereClause)
             | Vacuum
    deriving (Show, Read, Eq)

data ColumnType = ColumnTypeInt
                | ColumnTypeDouble
                | ColumnTypeVarchar Int
    deriving (Show, Read, Eq)

data Projection = All
                | Some [Selector]
    deriving (Show, Read, Eq)

data Selector = OneColumn ColumnSelector Alias
              | WholeTable TableName
    deriving (Show, Read, Eq)

data ColumnSelector = Column ColumnName
                    | QualifiedColumn TableName ColumnName
    deriving (Show, Read, Eq)

data WhereClause = And  WhereClause     WhereClause
                 | Or   WhereClause     WhereClause
                 | Gt   WhereSelector   WhereSelector
                 | Ge   WhereSelector   WhereSelector
                 | Eq   WhereSelector   WhereSelector
                 | Ne   WhereSelector   WhereSelector
                 | Le   WhereSelector   WhereSelector
                 | Lt   WhereSelector   WhereSelector
    deriving (Show, Read, Eq)

data WhereSelector = WhereSelectorColumn ColumnSelector
                   | WhereSelectorValue Value
    deriving (Show, Read, Eq)

data Value = ValueInt !Int
           | ValueDouble !Double
           | ValueString !String
    deriving (Show, Read, Eq)
