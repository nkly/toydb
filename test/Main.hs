module Main where

import Test.Hspec
import Parser
import Pager

main = hspec $ do
    describe "Parser" testParser
    describe "Pager" testPager

