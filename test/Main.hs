module Main where

import Test.Hspec
import qualified Cases.Parser
import qualified Cases.Serialization
import qualified Cases.Pager

main = hspec $ do
    describe "Parser" Cases.Parser.test
    describe "Serialization" Cases.Serialization.test
    describe "Pager" Cases.Pager.test

