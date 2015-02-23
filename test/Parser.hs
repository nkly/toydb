module Parser where

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language.Sql.Parser
import Language.Sql.Command


as    = OneColumn
col   = Column
qcol  = QualColumn

wc  = WSColumn . Column
wi  = WSValue . VInt
wd  = WSValue . VDouble
ws  = WSValue . VString

(&&&) = And
infixr 3 &&&
(|||) = Or
infixr 2 |||


testParse cmd exp = testCase (show cmd) $ assertEqual "" exp (parseCommand cmd)

testSelect = testGroup "SELECT"
    [ testParse
        "select * from test;"
        (Select All ["test"] Nothing Nothing)

    , testParse
        "select * from foo, bar;"
        (Select All ["foo", "bar"] Nothing Nothing)

    , testParse
        "select foo.*, bar.* from foo, bar;"
        (Select (Some [WholeTable "foo", WholeTable "bar"])
                ["foo", "bar"] Nothing Nothing)

    -- Testing column selectors

    , testParse
        "select a, b from test;"
        (Select (Some [col "a" `as` "a", col "b" `as` "b"])
                ["test"] Nothing Nothing)
    , testParse
        "select test.a from test;"
        (Select (Some [(qcol "test" "a") `as` "test.a"])
                ["test"] Nothing Nothing)
    , testParse
        "select a as b from test;"
        (Select (Some [col "a" `as` "b"])
                ["test"] Nothing Nothing)
    , testParse
        "select test.a as b from test;"
        (Select (Some [(qcol "test" "a") `as` "b"])
                ["test"] Nothing Nothing)

    -- Testing WHERE and LIMIT

    , testParse
        "select * from test where a > 10;"
        (Select All ["test"] (Just $ wc "a" `Gt` wi 10) Nothing)
    , testParse
        "select * from test where a > 10 and b < 20.0 or 'str' == a;"
        (Select All ["test"]
            (Just $ (wc "a" `Gt` wi 10)
                &&& (wc "b" `Lt` wd 20.0)
                ||| (ws "str" `Eq` wc "a"))
            Nothing)
    , testParse
        "select * from test where a > 10 and (b < 20.0 or 'str' == a);"
        (Select All ["test"]
            (Just $ (wc "a" `Gt` wi 10)
                &&& ((wc "b" `Lt` wd 20.0)
                        ||| (ws "str" `Eq` wc "a")))
            Nothing)
    , testParse
        "select * from test where a > 10 limit 5;"
        (Select All ["test"] (Just $ wc "a" `Gt` wi 10) (Just 5))
    ]

testCreateDrop = testGroup "CREATE and DROP"
    [ testParse
        "create table test (foo int, bar double, baz varchar(10));"
        (CreateTable "test"
            [("foo", ColInt), ("bar", ColDouble), ("baz", ColVarchar 10)])
    , testParse
        "create index test on test (foo, bar);"
        (CreateIndex "test" "test" ["foo", "bar"])
    , testParse
        "drop table test;"
        (DropTable "test")
    , testParse
        "drop index test;"
        (DropIndex "test")
    ]

testInsertUpdateDelete = testGroup "INSERT, UPDATE, DELETE"
    [ testParse
        "insert into test values (1, 20.0, 'some str');"
        (Insert "test" [VInt 1, VDouble 20.0, VString "some str"])
    , testParse
        "update test set (foo=1, bar=2.1, baz='some str');"
        (Update "test"
            [("foo", VInt 1), ("bar", VDouble 2.1), ("baz", VString "some str")]
            Nothing)
    , testParse
        "delete from test;"
        (Delete "test" Nothing)
    ]

testParser = testGroup "Parser"
    [ testSelect, testCreateDrop, testInsertUpdateDelete ]
