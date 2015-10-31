{-# LANGUAGE TypeFamilies #-}
module Database.Toy.Internal.Util.HasFileIO where

import Database.Toy.Internal.Prelude hiding (Handle)
import qualified Data.ByteString.Char8 as B
import qualified System.IO as S

class Monad m => HasFileIO m where
    type Handle m :: *
    hOpen :: S.FilePath -> S.IOMode -> m (Handle m)
    hGet  :: Handle m -> Int -> m ByteString
    hPut  :: Handle m -> ByteString -> m ()
    hSeek :: Handle m -> S.SeekMode -> Integer -> m ()

instance HasFileIO IO where
    type Handle IO = S.Handle
    hOpen = S.openBinaryFile
    hGet = B.hGet
    hPut = B.hPut
    hSeek = S.hSeek
