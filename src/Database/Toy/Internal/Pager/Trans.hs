{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}
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
    , chainPage
    , newPage
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Data.Serialize
import Data.Typeable
import Database.Toy.Internal.Prelude hiding (Handle)
import Database.Toy.Internal.Util.HasFileIO
import qualified System.IO as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Cache.LRU as LRU

import Database.Toy.Internal.Pager.Types


data PagerException = PageNotFound PageId
                    | PageCorrupted PageId
                    | PageOverflow PageId
    deriving (Show, Typeable)

instance Exception PagerException

-- | Run Pager action with given configuration and state
-- and return tuple with result and new pager state
runPager :: (HasFileIO m, MonadThrow m) => PagerConf -> PagerState -> PagerT m a -> m (a, PagerState)
runPager conf state action = do
    handle <- hOpen (view pagerFilePath conf) S.ReadWriteMode
    let internalState = PagerInternalState handle cache
    (result, (_, newState)) <- runStateT (runReaderT act conf) (internalState, state)
    return (result, newState)
  where
    dumpAllPages resultToReturn = do
        pagesToDump <- fmap (map snd . LRU.toList) getCache
        mapM_ dumpPage pagesToDump
        -- TODO: flush and close
        return resultToReturn
    maxPagesInMem = view pagerMaxPagesInMemory conf
    cache = LRU.newLRU $ Just $ fromIntegral maxPagesInMem
    PagerT act = action >>= dumpAllPages

-- | Read page by its id. If page is not loaded yet, then read it from
-- disc, probably causing LRU page to be dumped.
readPage :: (HasFileIO m, MonadThrow m) => PageId -> PagerT m Page
readPage pageId = do
    exists <- pageExists pageId
    if exists
        then do
            cached <- lookupInCache pageId
            maybe (readFromDisk pageId) return cached
        else
            throwM $ PageNotFound pageId

-- | Store given payload into page with given id. This action will
-- update page in memory. Changes will be written to disk when page is
-- unloaded.
--
-- Exception will be thrown if payload is too big for this page
writePage :: (HasFileIO m, MonadThrow m) => PageId -> ByteString -> PagerT m ()
writePage pageId newPayload = do
    pageSize <- asks (fromIntegral . view pagerPageSizeBytes)
    if (B.length newPayload > pagePayloadSize pageSize)
      then
        throwM $ PageOverflow pageId
      else do
        page <- readPage pageId
        insertPage $ set pagePayload newPayload page

-- | Set `destId` page to be the next after `sourceId` page in a
-- sense of linked list
chainPage :: (HasFileIO m, MonadThrow m) => PageId -> PageId -> PagerT m ()
chainPage sourceId destId = do
    source <- readPage sourceId
    destExists <- pageExists destId
    if destExists
        then insertPage $ set pageNextId destId source
        else throwM $ PageNotFound destId

-- | Request new empty page. If one of empty pages is available, then reuse it,
-- otherwise allocate new page at the end of database file.
newPage :: (HasFileIO m, MonadThrow m) => PagerT m Page
newPage = do
    lastFreePageId <- getExternalState $ view pagerFreelistStartId
    case lastFreePageId of
      NoPageId -> createNewPage
      pageId   -> reusePage pageId
  where
    createNewPage = do
        pageId <- fmap PageId $ getExternalState $ view pagerPagesCount
        page   <- clearPage $ Page pageId B.empty NoPageId
        insertPage page
        modifyExternalState (over pagerPagesCount succ)
        return page
    reusePage pageId = do
        page@(Page _ _ nextId) <- readPage pageId
        modifyExternalState (set pagerFreelistStartId nextId)
        emptiedPage <- clearPage page
        insertPage emptiedPage
        return emptiedPage
    clearPage :: Monad m => Page -> PagerT m Page
    clearPage (Page pageId _ _) = do
        (PagerConf _ pageSize _ _) <- ask
        let payload = B.replicate (fromIntegral $ pagePayloadSize pageSize) '\0'
        return (Page pageId payload NoPageId)


type Cache = LRU.LRU PageId Page

newtype PagerT m a = PagerT (ReaderT PagerConf (StateT (PagerInternalState m, PagerState) m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader PagerConf, MonadThrow)

instance MonadTrans PagerT where
    lift = PagerT . lift . lift

instance (Monad m, HasFileIO m) => HasFileIO (PagerT m) where
    type Handle (PagerT m) = Handle m
    hOpen f m = lift $ hOpen f m
    hGet h n = lift $ hGet h n
    hPut h s = lift $ hPut h s
    hSeek h m n = lift $ hSeek h m n

-- | Check whether page with given id exists in database
pageExists :: Monad m => PageId -> PagerT m Bool
pageExists NoPageId = error "Pager.pageExists: impossible page id"
pageExists (PageId pageId) = do
    pagesCount <- getExternalState $ view pagerPagesCount
    return (pageId < pagesCount)

-- | Read page with given id from disk and store it in the cache
readFromDisk :: (HasFileIO m, MonadThrow m) => PageId -> PagerT m Page
readFromDisk pageId = do
    pageSize <- asks $ view pagerPageSizeBytes
    handle   <- seekToPage pageId
    rawPage  <- hGet handle $ fromIntegral pageSize
    storeInCache rawPage
  where
    storeInCache rawPage = do
        page@(Page realPageId _ _) <- decodePage0 rawPage
        if realPageId /= pageId
            then
                throwM $ PageCorrupted pageId
            else do
                insertPage page
                return page
    decodePage0 rawPage = case decode rawPage of
        Right page -> return page
        Left _ -> throwM $ PageCorrupted pageId

-- | Seek database file to the place where page with given id starts
seekToPage :: HasFileIO m => PageId -> PagerT m (Handle m)
seekToPage NoPageId = error "Pager.seekToPage: impossible page id"
seekToPage (PageId pid) = do
    handle <- getHandle
    (PagerConf _ pageSize fileOffset _) <- ask
    let offset = toInteger $ fileOffset + pageSize * pid
    hSeek handle S.AbsoluteSeek offset
    return handle

-- | Lookup page with given id in cache. If page exists, this action
-- will mark it as most recently used.
lookupInCache :: Monad m => PageId -> PagerT m (Maybe Page)
lookupInCache pageId = do
    cache <- getCache
    case LRU.lookup pageId cache of
        (newCache, Just page) -> do
            setCache newCache
            return $ Just page
        (_, Nothing) ->
            return Nothing

-- | Insert or update page with given id in cache. If cache is overflown,
-- LRU page is dumped on disk.
insertPage :: HasFileIO m => Page -> PagerT m ()
insertPage page@(Page pageId _ _) = do
    (newCache, toDump) <- fmap (LRU.insertInforming pageId page) getCache
    maybe (return ()) (dumpPage . snd) toDump
    setCache newCache

-- | Write page to disk
dumpPage :: HasFileIO m => Page -> PagerT m ()
dumpPage page@(Page pageId _ _) = do
    handle <- seekToPage pageId
    hPut handle $ encode page

getCache :: Monad m => PagerT m Cache
getCache = PagerT $ gets (view pagerCache . fst)

setCache :: Monad m => Cache -> PagerT m ()
setCache = PagerT . modify . first . set pagerCache

getHandle :: Monad m => PagerT m (Handle m)
getHandle = PagerT $ gets (view pagerFileHandle . fst)

getExternalState :: Monad m => (PagerState -> a) -> PagerT m a
getExternalState fn = PagerT $ gets (fn . snd)

modifyExternalState :: Monad m => (PagerState -> PagerState) -> PagerT m ()
modifyExternalState fn = PagerT $ modify $ second fn
