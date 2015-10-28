{-# LANGUAGE TemplateHaskell #-}
module Database.Toy.Internal.Pager.Types where

import Control.Lens
import Data.Serialize
import Database.Toy.Internal.Prelude
import Database.Toy.Internal.Util.FixedSizeSerialize


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


data PagerConf = PagerConf
    { _pagerFileHandle       :: Handle
    , _pagerPageSizeBytes    :: Word32
    , _pagerOffsetBytes      :: Word32
    , _pagerMaxPagesInMemory :: Int
    }

makeLenses ''PagerConf

data PagerState = PagerState
    { _pagerPagesCount       :: Word32
    , _pagerFreelistStartId  :: PageId
    }

makeLenses ''PagerState
