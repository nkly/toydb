{-# LANGUAGE DeriveDataTypeable #-}
module Database.Toy.Internal.FileHeader
    ( FileHeader(..)
    , headerSizeBytes
    , readHeader
    , writeHeader
    , headerToPagerState
    , headerToPagerConf
    ) where

import Control.Exception
import Control.Monad
import Data.Serialize
import Data.Typeable
import Data.Word
import Database.Toy.Internal.Pager.Types
import System.IO
import qualified Data.ByteString.Char8 as B


data FileHeader = FileHeader
    { headerPageSize        :: Word32
    , headerPagesCount      :: Word32
    , headerFreelistStartId :: PageId
    }

data FileHeaderException = CantReadHeader String
    deriving (Show, Typeable)
instance Exception FileHeaderException

headerSizeBytes :: Num a => a
headerSizeBytes = 128

readHeader :: Handle -> IO FileHeader
readHeader handle = do
    headerBytes <- B.hGet handle headerSizeBytes
    either (throwIO . CantReadHeader) return $ decode headerBytes

writeHeader :: FileHeader -> Handle -> IO ()
writeHeader header handle =
    B.hPut handle $ encode header

headerToPagerState :: FileHeader -> PagerState
headerToPagerState (FileHeader _ pagesCount freelistStartId) =
    PagerState pagesCount freelistStartId

headerToPagerConf :: FileHeader -> Handle -> PagerConf
headerToPagerConf (FileHeader pageSize _ _) handle =
    PagerConf handle pageSize headerSizeBytes maxPagesInMemory

updateHeaderFromPagerState :: FileHeader -> PagerState -> FileHeader
updateHeaderFromPagerState fh (PagerState pagesCount freelistStartId) =
    fh { headerPagesCount = pagesCount, headerFreelistStartId = freelistStartId }



maxPagesInMemory :: Int
maxPagesInMemory = 100

fileHeaderMagicString :: B.ByteString
fileHeaderMagicString = B.append (B.pack "toydb") (B.replicate 3 '\0')

instance Serialize FileHeader where
    get = do
        magic <- getBytes 8
        when (magic /= fileHeaderMagicString) $
            fail "Wrong magic string"
        version         <- getWord32le
        pageSize        <- getWord32le
        pagesCount      <- getWord32le
        freelistStartId <- get
        return $ FileHeader pageSize pagesCount freelistStartId
    put (FileHeader pageSize pagesCount freelistStartId) = do
        put fileHeaderMagicString
        putWord32le 1 -- current version
        putWord32le pageSize
        putWord32le pagesCount
        put freelistStartId
        put (B.replicate 88 '\0')
