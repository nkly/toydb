module Language.Sql.Command where


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

data ColumnType = ColInt | ColDouble | ColVarchar Int
    deriving (Show, Read, Eq)

data Projection = All | Some [Selector]
    deriving (Show, Read, Eq)

data Selector = OneColumn ColumnSelector Alias | WholeTable TableName
    deriving (Show, Read, Eq)

data ColumnSelector = Column ColumnName | QualColumn TableName ColumnName
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

data WhereSelector = WSColumn ColumnSelector | WSValue Value
    deriving (Show, Read, Eq)

data Value = VInt Int | VDouble Double | VString String
    deriving (Show, Read, Eq)
