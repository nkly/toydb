module Database.Toy.Internal.Util.HasFileIO where

import Database.Toy.Internal.Prelude
import qualified Data.ByteString.Char8 as B
import qualified System.IO as S

class HasFileIO m where
    hGet :: Handle -> Int -> m ByteString
    hPut :: Handle -> ByteString -> m ()
    hSeek :: Handle -> S.SeekMode -> Integer -> m ()

instance HasFileIO IO where
    hGet = B.hGet
    hPut = B.hPut
    hSeek = S.hSeek
