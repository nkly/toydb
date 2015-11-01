module Pager where

import Control.Exception.Base (evaluate)
import Data.Serialize
import Data.Word
import Database.Toy.Internal.Pager.Trans
import Database.Toy.Internal.Pager.Types
import MockIO
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

testPagerReading = do
    it "should throw an error trying to read page with NoPageId" $
        (testReadPage pagerConf correctContents NoPageId) `shouldThrow` anyErrorCall
    it "should throw an exception trying to read non-existent page" $
        (testReadPage pagerConf correctContents $ PageId 42) `shouldThrow` anyPagerException
    it "should throw an exception trying to read page which id does not match given one" $
        (testReadPage pagerConf incorrectContents $ PageId 0) `shouldThrow` anyPagerException
    it "should correctly read existent page" $
        (testReadPage pagerConf correctContents $ PageId 0) `shouldReturn` True
    it "should correctly read existent page even with non-zero offset" $
        (testReadPage pagerConfWithOffset contentsWithOffset $ PageId 0) `shouldReturn` True
  where
    testReadPage pagerConf contents pageId = do
        let mockState = MockIOState contents 0
            readPageAction = fmap fst $
                runPager pagerConf pagerState $ readPage pageId
        page <- evalMockIO readPageAction mockState
        return $ page == correctPage
    pagerOffset :: Num a => a
    pagerOffset = 1
    pageSize = pageOverhead + 4
    pagerConf = PagerConf "" pageSize 0 1
    pagerState = PagerState 1 NoPageId
    pagerConfWithOffset = PagerConf "" pageSize pagerOffset 1
    correctPage = Page (PageId 0) (B.pack "TEST") NoPageId
    incorrectPage = Page (PageId 10) (B.pack "NOPE") NoPageId
    correctContents = encode correctPage
    incorrectContents = encode incorrectPage
    contentsWithOffset = B.append (B.replicate pagerOffset '\0') correctContents
    anyPagerException = const True :: Selector PagerException

testPagerWriting = do
    it "should throw an error trying to write page with NoPageId" pending
    it "should throw an exception trying to write overflown page" pending
    it "should write right page correctly" pending

-- Test chaining NoPageId, should throw an error
-- Test chaining to NoPageId, should set nextId and leave payload as is
-- Test creating new page with no free pages available,
--      should create page with id of (pagesNumber + 1)
-- Test creating new page with free pages available,
--      should create page with id of the first available page
--      in a freelist
-- Test Pager to satisfy "max pages in memory" criterion

testPager = do
    testPageIdSerialization
    testPageSerialization
    testPagerReading
    testPagerWriting
