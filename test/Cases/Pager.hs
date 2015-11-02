{-# LANGUAGE OverloadedStrings #-}
module Cases.Pager where

import Control.Exception.Base (evaluate, ErrorCall)
import Control.Monad
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

runMockPager :: B.ByteString -> PagerConf
             -> PagerState -> PagerT MockIO a
             -> IO (a, PagerState, B.ByteString)
runMockPager mockContents pagerConf pagerState action = do
    let mockState = MockIOState mockContents 0
    ((result, pagerState), newContents) <- runMockIO
        (runPager pagerConf pagerState action) mockState
    return (result, pagerState, newContents)

mockPageSize :: Num a => a
mockPageSize = pageOverhead + 4

mockFileName :: String
mockFileName = undefined -- Doesn't matter in MockIO

testPagerReading = do
    it "should throw an error trying to read page with NoPageId" $
        (readPage' correctContents pagerConf NoPageId) `shouldThrow` anyErrorCall
    it "should throw an exception trying to read non-existent page" $
        (readPage' correctContents pagerConf $ PageId 42) `shouldThrow` anyPagerException
    it "should throw an exception trying to read page which id does not match given one" $ do
        let incorrectPage = Page (PageId 10) "FAIL" NoPageId
            incorrectContents = encode incorrectPage
        (readPage' incorrectContents pagerConf $ PageId 0) `shouldThrow` anyPagerException
    it "should correctly read existent page" $ do
        page <- readPage' correctContents pagerConf $ PageId 0
        page `shouldBe` correctPage
    it "should correctly read existent page even with non-zero offset" $ do
        let offset :: Num a => a
            offset = 3
            pagerConfWithOffset = set pagerOffsetBytes 3 pagerConf
            contentsWithOffset = B.append (B.replicate offset '\0') correctContents
        page <- readPage' contentsWithOffset pagerConfWithOffset $ PageId 0
        page `shouldBe` correctPage
  where
    readPage' contents pagerConf pageId = do
        (page, _, _) <- runMockPager contents pagerConf pagerState (readPage pageId)
        return page
    pagerConf       = PagerConf mockFileName mockPageSize 0 1
    pagerState      = PagerState 1 NoPageId
    correctPage     = Page (PageId 0) "TEST" NoPageId
    correctContents = encode correctPage

testPagerWriting = do
    it "should throw an error trying to write page with NoPageId" $
        (writePage' NoPageId B.empty) `shouldThrow` (not . decodingFailedError)
    it "should throw an exception trying to write overflown page" $
        (writePage' (PageId 0) "OVERFLOW") `shouldThrow` anyPagerException
    it "should write right page correctly" $ do
        eitherPage <- writePage' (PageId 0) "PASS"
        eitherPage `shouldBe` (Right $ set pagePayload "PASS" originalPage)
  where
    writePage' :: PageId -> B.ByteString -> IO (Either String Page)
    writePage' pageId pageContents = do
        (_, _, newContents) <- runMockPager contents pagerConf pagerState $
            writePage pageId pageContents
        return $ decode newContents
    pagerConf       = PagerConf mockFileName mockPageSize 0 1
    pagerState      = PagerState 1 NoPageId
    originalPage    = Page (PageId 0) "TEST" NoPageId
    contents        = encode originalPage

testPagerChaining = do
    it "should throw an error trying to chain page with NoPageId given" $
        (chainPage' NoPageId (PageId 42)) `shouldThrow` (not . decodingFailedError)
    it "should throw an exception trying to chaing page to non-existent page" $
        (chainPage' (PageId 0) (PageId 42)) `shouldThrow` anyPagerException
    it "should chain page to NoPageId without changing its payload" $ do
        eitherPage <- chainPage' (PageId 0) (PageId 1)
        eitherPage `shouldBe` (Right $ set pageNextId (PageId 1) pageOne)
  where
    chainPage' :: PageId -> PageId -> IO (Either String Page)
    chainPage' sourceId destId = do
        (_, _, newContents) <- runMockPager contents pagerConf pagerState $
                chainPage sourceId destId
        return $ decode $ B.take mockPageSize newContents
    pagerConf = PagerConf mockFileName mockPageSize 0 1
    pagerState = PagerState 2 NoPageId
    pageOne = Page (PageId 0) "TEST" NoPageId
    pageTwo = Page (PageId 1) "TEST" NoPageId
    contents = B.concat $ map encode [pageOne, pageTwo]

testPagerNewPageCreation = do
    it "should create new page when no free pages available" $ do
        let pagerState    = PagerState 2 NoPageId
            expectedState = PagerState 3 NoPageId
            expectedPage  = Page (PageId 2) zeroPayload NoPageId
            expectedContents = B.concat $ map encode [pageOne, pageTwo, expectedPage]
        (page, newState, newContents) <- newPage' pagerState
        page        `shouldBe` expectedPage
        newState    `shouldBe` expectedState
        newContents `shouldBe` expectedContents
    it "should reuse free page when free pages available" $ do
        let pagerState    = PagerState 2 (PageId 1)
            expectedState = PagerState 2 NoPageId
            expectedPage  = Page (PageId 1) zeroPayload NoPageId
            expectedContents = B.concat $ map encode [pageOne, expectedPage]
        (page, newState, newContents) <- newPage' pagerState
        page        `shouldBe` expectedPage
        newState    `shouldBe` expectedState
        newContents `shouldBe` expectedContents
  where
    newPage' pagerState =
        runMockPager contents pagerConf pagerState newPage
    pagerConf   = PagerConf mockFileName mockPageSize 0 1
    pageOne     = Page (PageId 0) "TEST" NoPageId
    pageTwo     = Page (PageId 1) "TEST" NoPageId
    contents    = B.concat $ map encode [pageOne, pageTwo]
    zeroPayload = B.replicate (pagePayloadSize mockPageSize) '\0'

testPagerMemoryRestriction =
    it "should put in memory no more pages than set up in 'max pages in memory'" pending

test = do
    testPagerReading
    testPagerWriting
    testPagerChaining
    testPagerNewPageCreation
    testPagerMemoryRestriction
