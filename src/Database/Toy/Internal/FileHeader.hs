module Database.Toy.Internal.FileHeader
    ( FileHeader(..)
    , readHeader
    , writeHeader
    , headerToPagerState
    , headerToPagerConf
    ) where

import Data.Word
import Data.Serialize
import System.IO

import Database.Toy.Internal.Pager.Types

{-
    Database header spec:
        5 bytes -- magic string, "toydb"
        3 bytes -- file spec version (latest is 1)
        8 bytes -- overall header length in bytes
        8 bytes -- used page size in bytes
        8 bytes -- pages count in database
        8 bytes -- page id of first empty page or 0 if all pages are used
-}

data FileHeader = FileHeader
    { headerHandle          :: Handle
    , headerVersion         :: Int
    , headerSize            :: Word32
    , headerPageSize        :: Word32
    , headerPagesCount      :: Word32
    , headerFreelistStartId :: PageId
    }

readHeader :: Handle -> IO FileHeader
readHeader handle = undefined

writeHeader :: FileHeader -> IO ()
writeHeader header = undefined

headerToPagerState :: FileHeader -> PagerState
headerToPagerState (FileHeader _ _ _ _ pagesCount freelistStartId) =
    PagerState pagesCount freelistStartId

headerToPagerConf :: FileHeader -> PagerConf
headerToPagerConf (FileHeader handle _ headerSize pageSize _ _) =
    PagerConf handle pageSize headerSize maxPagesInMemory

updateHeaderFromPagerState :: FileHeader -> PagerState -> FileHeader
updateHeaderFromPagerState fh (PagerState pagesCount freelistStartId) =
    fh { headerPagesCount = pagesCount, headerFreelistStartId = freelistStartId }

maxPagesInMemory :: Int
maxPagesInMemory = 100
