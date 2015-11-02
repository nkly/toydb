{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Utils.MockIO where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.State (StateT)
import Control.Monad.State.Class (MonadState)
import Database.Toy.Internal.Util.HasFileIO
import qualified Data.ByteString.Char8 as B
import qualified Control.Monad.State as ST
import qualified System.IO as S

data MockIOState = MockIOState
    { mockContents :: B.ByteString
    , mockCurrentPos :: Int
    }

newtype MockIO a = MockIO (StateT MockIOState IO a)
    deriving (Functor, Applicative, Monad, MonadState MockIOState)

instance HasFileIO MockIO where
    type Handle MockIO = ()
    hOpen _ _ = return ()
    hClose _  = return ()
    hGet _ n = do
        pos <- ST.gets mockCurrentPos
        len <- fmap B.length $ ST.gets mockContents
        if pos > len
            then error "End of file"
            else fmap (B.take n . B.drop pos) $ ST.gets mockContents
    hPut _ str = do
        pos <- ST.gets mockCurrentPos
        contents <- ST.gets mockContents
        let diff = pos - B.length contents
            paddedContents = if diff > 0
                then B.append contents $ B.replicate diff '\0'
                else contents
            (cBefore, cAfter) = B.splitAt pos paddedContents
            lenToWrite = B.length str
            newAfter = B.append str $ B.drop lenToWrite cAfter
        ST.modify (\s -> s { mockContents = B.append cBefore newAfter })
    hSeek _ mode offset = do
        let offsetInt = fromInteger offset
            action = case mode of
                S.AbsoluteSeek -> \s -> s { mockCurrentPos = offsetInt }
                S.RelativeSeek -> \s -> s { mockCurrentPos = mockCurrentPos s + offsetInt }
                S.SeekFromEnd  -> \s -> s { mockCurrentPos = mockCurrentPos s - offsetInt }
        ST.modify action

instance MonadThrow MockIO where
    throwM exc = MockIO $ throwM exc

evalMockIO :: MockIO a -> MockIOState -> IO a
evalMockIO action = fmap fst . runMockIO action

execMockIO :: MockIO a -> MockIOState -> IO B.ByteString
execMockIO action = fmap snd . runMockIO action

runMockIO :: MockIO a -> MockIOState -> IO (a, B.ByteString)
runMockIO (MockIO action) = fmap (second mockContents) . ST.runStateT action
