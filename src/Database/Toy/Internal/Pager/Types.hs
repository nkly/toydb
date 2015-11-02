{-# LANGUAGE TemplateHaskell #-}
module Database.Toy.Internal.Pager.Types where

import Control.Lens
import Data.Serialize
import Data.Cache.LRU (LRU)
import Database.Toy.Internal.Prelude hiding (Handle)
import Database.Toy.Internal.Util.FixedSizeSerialize
import Database.Toy.Internal.Util.HasFileIO


data PageId = PageId Word32 | NoPageId
    deriving (Show, Eq, Ord)

instance Serialize PageId where
    get = do
        id <- getWord32le
        return $ if id == maxBound then NoPageId else PageId id
    put NoPageId    = putWord32le maxBound
    put (PageId id) =
        if id == maxBound
            then error "PageId.put: maxBound is reserved for NoPageId"
            else putWord32le id

instance FixedSizeSerialize PageId where
    serializedSize _ = serializedSize (0 :: Word32)


data Page = Page
    { _pageId        :: PageId
    , _pagePayload   :: ByteString
    , _pageNextId    :: PageId
    } deriving (Show, Eq)

makeLenses ''Page

instance Serialize Page where
    get = do
        pid     <- get
        npid    <- get
        payload <- remaining >>= getBytes
        return $ Page pid payload npid
    put (Page NoPageId _ _) =
        error "Page.get: impossible page id"
    put (Page pageId payload nextId) = do
        put pageId
        put nextId
        putByteString payload

pageOverhead :: Num a => a
pageOverhead = 2 * fromIntegral (serializedSize NoPageId)

pagePayloadSize :: Num a => a -> a
pagePayloadSize pageSize = pageSize - pageOverhead

data PagerConf = PagerConf
    { _pagerFilePath         :: FilePath
    , _pagerPageSizeBytes    :: Word32
    , _pagerOffsetBytes      :: Word32
    , _pagerMaxPagesInMemory :: Int
    } deriving (Eq, Show)

makeLenses ''PagerConf

data PagerState = PagerState
    { _pagerPagesCount       :: Word32
    , _pagerFreelistStartId  :: PageId
    } deriving (Eq, Show)

makeLenses ''PagerState

data PagerInternalState m = PagerInternalState
    { _pagerFileHandle :: Handle m
    , _pagerCache :: LRU PageId Page
    }

makeLenses ''PagerInternalState
