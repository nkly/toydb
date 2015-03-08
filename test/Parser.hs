module Parser where

import Test.Hspec

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


shouldBeParsedAs cmd exp =
    it ("should parse " ++ show cmd) $
        exp `shouldBe` (parseCommand cmd)

testSelect = do
    "select * from test;" `shouldBeParsedAs`
        (Select All ["test"] Nothing Nothing)
    "select * from foo, bar;" `shouldBeParsedAs`
        (Select All ["foo", "bar"] Nothing Nothing)
    "select foo.*, bar.* from foo, bar;" `shouldBeParsedAs`
        (Select (Some [WholeTable "foo", WholeTable "bar"])
                ["foo", "bar"] Nothing Nothing)

    -- Testing column selectors
    "select a, b from test;" `shouldBeParsedAs`
        (Select (Some [col "a" `as` "a", col "b" `as` "b"])
                ["test"] Nothing Nothing)
    "select test.a from test;" `shouldBeParsedAs`
        (Select (Some [(qcol "test" "a") `as` "test.a"])
                ["test"] Nothing Nothing)
    "select a as b from test;" `shouldBeParsedAs`
        (Select (Some [col "a" `as` "b"])
                ["test"] Nothing Nothing)
    "select test.a as b from test;" `shouldBeParsedAs`
        (Select (Some [(qcol "test" "a") `as` "b"])
                ["test"] Nothing Nothing)

    -- Testing WHERE and LIMIT
    "select * from test where a > 10;" `shouldBeParsedAs`
        (Select All ["test"] (Just $ wc "a" `Gt` wi 10) Nothing)
    "select * from test where a > 10 and b < 20.0 or 'str' == a;"
        `shouldBeParsedAs`
            (Select All ["test"]
                (Just $ (wc "a" `Gt` wi 10)
                    &&& (wc "b" `Lt` wd 20.0)
                    ||| (ws "str" `Eq` wc "a"))
                Nothing)
    "select * from test where a > 10 and (b < 20.0 or 'str' == a);"
        `shouldBeParsedAs`
            (Select All ["test"]
                (Just $ (wc "a" `Gt` wi 10)
                    &&& ((wc "b" `Lt` wd 20.0)
                            ||| (ws "str" `Eq` wc "a")))
                Nothing)
    "select * from test where a > 10 limit 5;" `shouldBeParsedAs`
        (Select All ["test"] (Just $ wc "a" `Gt` wi 10) (Just 5))

testCreateDrop = do
    "create table test (foo int, bar double, baz varchar(10));"
        `shouldBeParsedAs`
            (CreateTable "test"
                [("foo", ColInt), ("bar", ColDouble), ("baz", ColVarchar 10)])
    "create index test on test (foo, bar);" `shouldBeParsedAs`
        (CreateIndex "test" "test" ["foo", "bar"])
    "drop table test;" `shouldBeParsedAs`
        (DropTable "test")
    "drop index test;" `shouldBeParsedAs`
        (DropIndex "test")

testInsertUpdateDelete = do
    "insert into test values (1, 20.0, 'some str');" `shouldBeParsedAs`
        (Insert "test" [VInt 1, VDouble 20.0, VString "some str"])
    "update test set (foo=1, bar=2.1, baz='some str');" `shouldBeParsedAs`
        (Update "test"
            [("foo", VInt 1), ("bar", VDouble 2.1), ("baz", VString "some str")]
            Nothing)
    "delete from test;" `shouldBeParsedAs`
        (Delete "test" Nothing)

testParser = do
    testSelect
    testCreateDrop
    testInsertUpdateDelete
