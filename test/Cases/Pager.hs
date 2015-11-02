{-# LANGUAGE OverloadedStrings #-}
module Cases.Pager where

import Control.Exception.Base (evaluate, ErrorCall)
import Control.Lens
import Data.Serialize
import Database.Toy.Internal.Pager.Trans
import Database.Toy.Internal.Pager.Types
import Utils.MockIO
import Test.Hspec
import qualified Data.ByteString.Char8 as B


anyPagerException :: Selector PagerException
anyPagerException = const True

decodingFailedError :: Selector ErrorCall
decodingFailedError = errorCall "Decoding failed"

decodingFailed :: a
decodingFailed = error "Decoding failed"

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
    correctPage = Page (PageId 0) "TEST" NoPageId
    incorrectPage = Page (PageId 10) "FAIL" NoPageId
    correctContents = encode correctPage
    incorrectContents = encode incorrectPage
    contentsWithOffset = B.append (B.replicate pagerOffset '\0') correctContents

testPagerWriting = do
    it "should throw an error trying to write page with NoPageId" $
        (testWritePage NoPageId B.empty) `shouldThrow` (not . decodingFailedError)
    it "should throw an exception trying to write overflown page" $
        (testWritePage (PageId 0) "OVERFLOW") `shouldThrow` anyPagerException
    it "should write right page correctly" $
        (testWritePage (PageId 0) "PASS") `shouldReturn` True
  where
    testWritePage pageId pageContents = do
        let originalPage = Page (PageId 0) "TEST" NoPageId
            contents = encode originalPage
            mockState = MockIOState contents 0
            writePageAction = fmap fst $
                runPager pagerConf pagerState $ writePage pageId pageContents
            checkPageContentsApplied newPage =
                newPage == set pagePayload pageContents originalPage
        eitherPage <- fmap decode $ execMockIO writePageAction mockState
        return $ either (const decodingFailed) checkPageContentsApplied eitherPage
    pageSize = pageOverhead + 4
    pagerConf = PagerConf "" pageSize 0 1
    pagerState = PagerState 1 NoPageId

testPagerChaining = do
    it "should throw an error trying to chain page with NoPageId given" $
        (testChainPage NoPageId (PageId 42)) `shouldThrow` (not . decodingFailedError)
    it "should throw an exception trying to chaing page to non-existent page" $
        (testChainPage (PageId 0) (PageId 42)) `shouldThrow` anyPagerException
    it "should chain page to NoPageId without changing its payload" $
        (testChainPage (PageId 0) (PageId 1)) `shouldReturn` True
  where
    testChainPage sourceId destId = do
        let pageOne = Page (PageId 0) "TEST" NoPageId
            pageTwo = Page (PageId 1) "TEST" NoPageId
            contents = B.concat $ map encode [pageOne, pageTwo]
            mockState = MockIOState contents 0
            chainPageAction = fmap fst $
                runPager pagerConf pagerState $ chainPage sourceId destId
            checkPageContentsRemained newPage =
                newPage == set pageNextId (PageId 1) pageOne
        eitherPage <- fmap (decode . B.take pageSize) $ execMockIO chainPageAction mockState
        return $ either (const decodingFailed) checkPageContentsRemained eitherPage
    pageSize :: Num a => a
    pageSize = pageOverhead + 4
    pagerConf = PagerConf "" pageSize 0 1
    pagerState = PagerState 2 NoPageId

testPagerCreating = do
    it "should create new page when no free pages available" pending
    it "should reuse free page when free pages available" pending

testPagerRestriction =
    it "should put in memory no more pages than set up in 'max pages in memory'" pending

test = do
    testPagerReading
    testPagerWriting
    testPagerChaining
    testPagerCreating
    testPagerRestriction
