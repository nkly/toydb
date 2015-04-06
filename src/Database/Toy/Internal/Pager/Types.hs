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

instance Hashable PageId where
    hashWithSalt salt NoPageId = error "NoPageId should not be hashed"
    hashWithSalt salt (PageId pid) = hashWithSalt salt pid

data Page = Page
    { _pageId        :: PageId
    , _pagePayload   :: B.ByteString
    , _pageNextId    :: PageId
    }

makeLenses ''Page

pageOverhead :: Num a => a
pageOverhead = 16

encodePage :: Page -> B.ByteString
encodePage (Page NoPageId _ _) = error "Page.encodePage: impossible page id"
encodePage (Page (PageId pid) payload nextId) = runPut $ do
    putWord32le pid
    putByteString payload
    case nextId of
        NoPageId -> putWord32le 0
        PageId npid -> putWord32le npid

decodePage :: B.ByteString -> Either String Page
decodePage str = (flip runGet) str $ do
    pid <- fmap PageId getWord32le
    payload <- getBytes (B.length str - pageOverhead)
    npid <- fmap (\x -> if x == 0 then NoPageId else PageId x) getWord32le
    return $ Page pid payload npid

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
