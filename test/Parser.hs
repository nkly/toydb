module Parser where

import Database.Toy.Internal.Language.Command
import Database.Toy.Internal.Language.Parser
import Test.Hspec


as    = OneColumn
col   = Column
qcol  = QualifiedColumn

wc  = WhereSelectorColumn . Column
wi  = WhereSelectorValue  . ValueInt
wd  = WhereSelectorValue  . ValueDouble
ws  = WhereSelectorValue  . ValueString

i = ValueInt
d = ValueDouble
s = ValueString

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
                [("foo", ColumnTypeInt),
                 ("bar", ColumnTypeDouble),
                 ("baz", ColumnTypeVarchar 10)])
    "create index test on test (foo, bar);" `shouldBeParsedAs`
        (CreateIndex "test" "test" ["foo", "bar"])
    "drop table test;" `shouldBeParsedAs`
        (DropTable "test")
    "drop index test;" `shouldBeParsedAs`
        (DropIndex "test")

testInsertUpdateDelete = do
    "insert into test values (1, 20.0, 'some str');" `shouldBeParsedAs`
        (Insert "test" [i 1, d 20.0, s "some str"])
    "update test set (foo=1, bar=2.1, baz='some str');" `shouldBeParsedAs`
        (Update "test"
            [("foo", i 1), ("bar", d 2.1), ("baz", s "some str")]
            Nothing)
    "delete from test;" `shouldBeParsedAs`
        (Delete "test" Nothing)

testParser = do
    testSelect
    testCreateDrop
    testInsertUpdateDelete
