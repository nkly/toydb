module Main where

import Test.Framework.Runners.Console
import TestParser

main = defaultMain [testParser]

