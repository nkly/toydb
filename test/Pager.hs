module Pager where

import Control.Exception.Base (evaluate)
import Data.Serialize
import Data.Word
import Database.Toy.Internal.Pager.Trans
import Database.Toy.Internal.Pager.Types
import Test.Hspec
import qualified Data.ByteString.Char8 as B


shouldBeEncodedTo entity expectation =
    it ("'" ++ show entity ++ "' should be encoded") $
        encode entity `shouldBe` expectation

shouldBeDecodedTo bytes expectation =
    it ("'" ++ show expectation ++ "' should be decoded") $
        decode bytes `shouldBe` Right expectation

testPageIdSerialization = do
    NoPageId  `shouldBeEncodedTo` (runPut $ putWord32le maxBound)
    PageId 42 `shouldBeEncodedTo` (runPut $ putWord32le 42)
    (runPut $ putWord32le maxBound) `shouldBeDecodedTo` NoPageId
    (runPut $ putWord32le 42)       `shouldBeDecodedTo` PageId 42
    it "'PageId maxBound' should not be encoded" $
        (evaluate $ encode $ PageId maxBound) `shouldThrow` anyErrorCall


testPageSerialization = do
    testPage `shouldBeEncodedTo` testPageBytes
    testPageBytes `shouldBeDecodedTo` testPage
    it "Page with NoPageId should not be encoded" $
        (evaluate $ encode $ Page NoPageId testPayload (PageId 0)) `shouldThrow` anyErrorCall
  where
    testPayload = B.pack "TestPayload"
    testPage = Page (PageId 0) testPayload NoPageId
    testPageBytes = B.concat [ encode $ PageId 0
                             , encode NoPageId
                             , testPayload
                             ]

testPagerReading =
    it "should read pages" pending

testPagerWriting =
    it "should write pages" pending

testPager = do
    testPageIdSerialization
    testPageSerialization
    testPagerReading
    testPagerWriting
