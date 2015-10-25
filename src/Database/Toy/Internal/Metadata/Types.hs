{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Database.Toy.Internal.Metadata.Types where

import Control.Lens
import Data.Serialize
import Database.Toy.Internal.Prelude
import Database.Toy.Internal.Pager.Types (PageId)
import Database.Toy.Internal.Util.FixedSizeSerialize
import qualified Data.ByteString.Char8 as B


-- | Contains internal database related information.
-- See `doc/architecture.mkdn` file.
data DatabaseMetadata = DatabaseMetadata
    { _metaFileSpecVersion :: Word8
    , _metaPageSize :: Word16
    , _metaPagesNumber :: Word32
    , _metaFirstEmptyPageId :: PageId
    , _metaTablesMetaPageId :: PageId
    , _metaIndexesMetaPageId :: PageId
    }

makeLenses ''DatabaseMetadata

instance Serialize DatabaseMetadata where
    get = do
        magic <- getBytes 5
        when (magic /= B.pack "toydb") $ fail "Wrong magic string"
        version     <- getWord8
        pageSize    <- getWord16le
        pagesNumber <- getWord32le
        firstEmptyPageId    <- get
        tablesMetaPageId    <- get
        indexesMetaPageId   <- get
        skip bytesLeft
        return $ DatabaseMetadata version pageSize pagesNumber
                    firstEmptyPageId tablesMetaPageId indexesMetaPageId
      where
        bytesLeft = 64 - (5 + 1 + 2 + 4 + 4 + 4 + 4)
    put (DatabaseMetadata version pageSize pagesNumber
            firstEmptyPageId tablesMetaPageId indexesMetaPageId) = do
        put "toydb"
        putWord8 version
        putWord16le pageSize
        putWord32le pagesNumber
        put firstEmptyPageId
        put tablesMetaPageId
        put indexesMetaPageId
        putByteString $ B.replicate bytesLeft '\0'
      where
        bytesLeft = 64 - (5 + 1 + 2 + 4 + 4 + 4 + 4)

instance FixedSizeSerialize DatabaseMetadata where
    serializedSize _ = 64
