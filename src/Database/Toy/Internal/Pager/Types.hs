{-# LANGUAGE TemplateHaskell #-}
module Database.Toy.Internal.Pager.Types where

import Control.Lens
import Data.Hashable
import Data.Word
import Data.Serialize
import Data.Time.Clock.POSIX
import System.IO
import qualified Data.ByteString as B
import qualified Data.HashTable.IO as HT


data PageId = PageId Word32 | NoPageId
    deriving (Show, Eq)

instance Serialize PageId where
    get = do
        id <- getWord32le
        return $ if id == -1 then NoPageId else PageId id
    put NoPageId    = putWord32le (-1)
    put (PageId id) = putWord32le id

instance Hashable PageId where
    hashWithSalt salt NoPageId = error "PageId.hashWithSalt: NoPageId should not be hashed"
    hashWithSalt salt (PageId pid) = hashWithSalt salt pid

data Page = Page
    { _pageId        :: PageId
    , _pagePayload   :: B.ByteString
    , _pageNextId    :: PageId
    }

makeLenses ''Page

pageOverhead :: Num a => a
pageOverhead = 16

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

type PageMemStorage = HT.LinearHashTable PageId (Page, POSIXTime)

data InternalState = InternalState
    { _pagerStorage      :: PageMemStorage
    , _pagerStorageSize  :: Int
    }

makeLenses ''InternalState
