module Main where

import Test.Hspec
import Parser

main = hspec $ do
    describe "Parser" testParser

