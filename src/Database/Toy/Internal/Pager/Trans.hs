{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Database.Toy.Internal.Pager.Trans
    ( PagerConf(..)
    , PagerState(..)
    , PagerException(..)
    , PageId(..)
    , Page(..)
    , PagerT
    , runPager
    , readPage
    , writePage
    , newPage
    ) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Serialize
import Data.Time.Clock.POSIX
import Data.Typeable
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.HashTable.IO as HT

import Database.Toy.Internal.Pager.Types


data PagerException = PageNotFound PageId
                    | PageCorrupted PageId
                    | PageOverflow PageId
    deriving (Show, Typeable)
instance Exception PagerException

runPager :: MonadIO m => PagerConf -> PagerState -> PagerT m a -> m (a, PagerState)
runPager conf state (PagerT act) = do
    let handle = view pagerFileHandle conf
        pageSize = fromIntegral $ view pagerPageSizeBytes conf
    liftIO $ do
        hSetBuffering handle $ BlockBuffering $ Just pageSize
        hSetBinaryMode handle True
    pagerStorage <- liftIO $
        HT.newSized (view pagerMaxPagesInMemory conf)
    let internalState = InternalState pagerStorage 0
    (result, (_, newState)) <- runStateT (runReaderT act conf) (internalState, state)
    return (result, newState)

readPage :: MonadIO m => PageId -> PagerT m Page
readPage pageId = pageExists pageId >>= readIfExists
  where
    readIfExists False = liftIO $ throwIO $ PageNotFound pageId
    readIfExists True  = do
        cached <- lookupInCache pageId
        maybe readAndStore return cached
    readAndStore = do
        pageData <- readFromDisk pageId
        storeInCache pageId pageData

writePage :: MonadIO m => PageId -> B.ByteString -> PagerT m ()
writePage pageId toWrite = do
    pageSize <- asks (fromIntegral . view pagerPageSizeBytes)
    if (B.length toWrite > pageSize - pageOverhead)
      then liftIO $ throwIO $ PageOverflow pageId
      else do
        page <- readPage pageId
        insertWithTimestamp pageId $ set pagePayload toWrite page

chainPage :: MonadIO m => PageId -> PageId -> PagerT m ()
chainPage pageId chainTo = do
    page <- readPage pageId
    insertWithTimestamp pageId $ set pageNextId chainTo page

newPage :: MonadIO m => PagerT m Page
newPage = do
    lastFreePageId <- getExternalState $ view pagerFreelistStartId
    case lastFreePageId of
      NoPageId -> createNewPage
      pageId   -> reusePage pageId
  where
    reusePage pageId = do
        (Page _ payload nextId) <- readPage pageId
        modifyExternalState (set pagerFreelistStartId nextId)
        return (Page pageId payload NoPageId)



newtype PagerT m a = PagerT (ReaderT PagerConf (StateT (InternalState, PagerState) m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader PagerConf)

instance MonadTrans PagerT where
    lift = PagerT . lift . lift

pageExists :: MonadIO m => PageId -> PagerT m Bool
pageExists NoPageId = error "Pager.pageExists: impossible page id"
pageExists (PageId pageId) = do
    pagesCount <- getExternalState $ view pagerPagesCount
    return (pageId < pagesCount)

readFromDisk :: MonadIO m => PageId -> PagerT m B.ByteString
readFromDisk pageId = do
    pageSize <- asks $ view pagerPageSizeBytes
    handle <- seekToPage pageId
    liftIO $ B.hGet handle $ fromIntegral pageSize

writeToDisk :: MonadIO m => PageId -> PagerT m ()
writeToDisk pageId = do
    cache <- getInternalState $ view pagerStorage
    (page,_) <- liftIO $ do
        result <- HT.lookup cache pageId
        maybe (throwIO $ PageNotFound pageId) return result
    handle <- seekToPage pageId
    liftIO $ B.hPut handle $ encode page

seekToPage :: MonadIO m => PageId -> PagerT m Handle
seekToPage NoPageId = error "Pager.seekToPage: impossible page id"
seekToPage (PageId pid) = do
    (PagerConf handle pageSize fileOffset _) <- ask
    let offset = toInteger $ fileOffset + pageSize * pid
    liftIO $ do
        hSeek handle AbsoluteSeek offset
        return handle

storeInCache :: MonadIO m => PageId -> B.ByteString -> PagerT m Page
storeInCache pageId rawPage = do
    page@(Page realPageId _ _) <- decodePage0
    if realPageId /= pageId
      then liftIO $ throwIO $ PageCorrupted pageId
      else do
        cache <- getInternalState $ view pagerStorage
        insertWithTimestamp pageId page
        modifyInternalState (over pagerStorageSize succ)
        return page
  where
    decodePage0 = case decode rawPage of
        Right page -> return page
        Left _ -> liftIO $ throwIO $ PageCorrupted pageId

lookupInCache :: MonadIO m => PageId -> PagerT m (Maybe Page)
lookupInCache pageId = do
    cache  <- getInternalState $ view pagerStorage
    result <- liftIO $ HT.lookup cache pageId
    case result of
        Just (page, _) -> do
            insertWithTimestamp pageId page
            return $ Just page
        Nothing ->
            return Nothing

insertWithTimestamp :: MonadIO m => PageId -> Page -> PagerT m ()
insertWithTimestamp pageId page = do
    (InternalState cache cacheSize) <- getInternalState id
    maxPagesInMem <- asks $ view pagerMaxPagesInMemory
    isInCache <- liftIO $ isJust <$> HT.lookup cache pageId
    when (not isInCache && cacheSize == maxPagesInMem) dumpLRUPage
    liftIO $ do
        timestamp <- getPOSIXTime
        HT.insert cache pageId (page, timestamp)

dumpLRUPage :: MonadIO m => PagerT m ()
dumpLRUPage = do
    now <- liftIO getPOSIXTime
    cache <- getInternalState $ view pagerStorage
    (pageToDump, _) <- liftIO $ HT.foldM lruPredicate (NoPageId, now) cache
    writeToDisk pageToDump
    liftIO $ HT.delete cache pageToDump
    modifyInternalState (over pagerStorageSize pred)
  where
    lruPredicate (pid0, time0) (pid1, (_, time1))
        | time0 < time1 = return (pid0, time0)
        | otherwise     = return (pid1, time1)

createNewPage :: MonadIO m => PagerT m Page
createNewPage = do
    pagesCount <- getExternalState $ view pagerPagesCount
    (PagerConf handle pageSize _ _) <- ask
    let payload = B.replicate (fromIntegral pageSize - pageOverhead) 0
        page = Page (PageId pagesCount) payload NoPageId
    insertWithTimestamp (view pageId page) page
    liftIO $ do
        hSeek handle SeekFromEnd 0
        B.hPut handle $ encode page
    modifyInternalState (over pagerStorageSize succ)
    modifyExternalState (over pagerPagesCount succ)
    return page

getInternalState :: Monad m => (InternalState -> a) -> PagerT m a
getInternalState fn = PagerT $ gets (fn . fst)

modifyInternalState :: Monad m => (InternalState -> InternalState) -> PagerT m ()
modifyInternalState fn = PagerT $ modify $ \(is, es) -> (fn is, es)

getExternalState :: Monad m => (PagerState -> a) -> PagerT m a
getExternalState fn = PagerT $ gets (fn . snd)

modifyExternalState :: Monad m => (PagerState -> PagerState) -> PagerT m ()
modifyExternalState fn = PagerT $ modify $ \(is, es) -> (is, fn es)
