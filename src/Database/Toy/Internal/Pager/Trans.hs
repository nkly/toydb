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

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Data.Serialize
import Data.Time.Clock.POSIX
import Data.Typeable
import Database.Toy.Internal.Prelude
import Database.Toy.Internal.Util.HasFileIO
import System.IO hiding (hSeek)
import qualified Data.ByteString as B
import qualified Data.Cache.LRU as LRU

import Database.Toy.Internal.Pager.Types


data PagerException = PageNotFound PageId
                    | PageCorrupted PageId
                    | PageOverflow PageId
    deriving (Show, Typeable)

instance Exception PagerException

-- | Run Pager action with given configuration and state
-- and return tuple with result and new pager state
runPager :: PagerIO m => PagerConf -> PagerState -> PagerT m a -> m (a, PagerState)
runPager conf state (PagerT act) = do
    let maxPagesInMem = view pagerMaxPagesInMemory conf
        cache = LRU.newLRU $ Just $ fromIntegral maxPagesInMem
    (result, (_, newState)) <- runStateT (runReaderT act conf) (cache, state)
    -- TODO: dump the rest of pages
    return (result, newState)

-- | Read page by its id. If page is not loaded yet, then read it from
-- disc, probably causing LRU page to be dumped.
readPage :: PagerIO m => PageId -> PagerT m Page
readPage pageId = pageExists pageId >>= readIfExists
  where
    readIfExists False = lift $ throwM $ PageNotFound pageId
    readIfExists True  = do
        cached <- lookupInCache pageId
        maybe (readFromDisk pageId) return cached

-- | Store given payload into page with given id. This action will
-- update page in memory. Changes will be written to disk when page is
-- unloaded.
--
-- Exception will be thrown if payload is too big for this page
writePage :: PagerIO m => PageId -> ByteString -> PagerT m ()
writePage pageId newPayload = do
    pageSize <- asks (fromIntegral . view pagerPageSizeBytes)
    if (B.length newPayload > pageSize - pageOverhead)
      then
        lift $ throwM $ PageOverflow pageId
      else do
        page <- readPage pageId
        insertPage pageId $ set pagePayload newPayload page

-- | Attach one page to another in a sense of linked list
chainPage :: PagerIO m => PageId -> PageId -> PagerT m ()
chainPage pageId chainTo = do
    page <- readPage pageId
    insertPage pageId $ set pageNextId chainTo page

-- | Request new empty page. If one of empty pages is available, then reuse it,
-- otherwise allocate new page at the end of database file.
newPage :: PagerIO m => PagerT m Page
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


type Cache = LRU.LRU PageId Page

newtype PagerT m a = PagerT (ReaderT PagerConf (StateT (Cache, PagerState) m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader PagerConf)

instance MonadTrans PagerT where
    lift = PagerT . lift . lift

class (HasFileIO m, MonadThrow m) => PagerIO m
instance PagerIO IO

-- | Check whether page with given id exists in database
pageExists :: PagerIO m => PageId -> PagerT m Bool
pageExists NoPageId = error "Pager.pageExists: impossible page id"
pageExists (PageId pageId) = do
    pagesCount <- getExternalState $ view pagerPagesCount
    return (pageId < pagesCount)

-- | Read page with given id from disk and store it in the cache
readFromDisk :: PagerIO m => PageId -> PagerT m Page
readFromDisk pageId = do
    pageSize <- asks $ view pagerPageSizeBytes
    handle <- seekToPage pageId
    rawPage <- lift $ hGet handle $ fromIntegral pageSize
    storeInCache rawPage
  where
    storeInCache rawPage = do
        page@(Page realPageId _ _) <- decodePage0 rawPage
        if realPageId /= pageId
            then
                lift $ throwM $ PageCorrupted pageId
            else do
                insertPage pageId page
                return page
    decodePage0 rawPage = case decode rawPage of
        Right page -> return page
        Left _ -> lift $ throwM $ PageCorrupted pageId

-- | Seek database file to the place where page with given id starts
seekToPage :: PagerIO m => PageId -> PagerT m Handle
seekToPage NoPageId = error "Pager.seekToPage: impossible page id"
seekToPage (PageId pid) = do
    (PagerConf handle pageSize fileOffset _) <- ask
    let offset = toInteger $ fileOffset + pageSize * pid
    lift $ hSeek handle AbsoluteSeek offset
    return handle

-- | Lookup page with given id in cache. If page exists, this action
-- will mark it as most recently used.
lookupInCache :: PagerIO m => PageId -> PagerT m (Maybe Page)
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
insertPage :: PagerIO m => PageId -> Page -> PagerT m ()
insertPage pageId page = do
    (newCache, toDump) <- fmap (LRU.insertInforming pageId page) getCache
    maybe (return ()) (dumpPage . snd) toDump
    setCache newCache
  where
    dumpPage page@(Page pageId _ _) = do
        handle <- seekToPage pageId
        lift $ hPut handle $ encode page

-- | Allocate new page filled with zeroes in database file and load it into
-- cache
createNewPage :: PagerIO m => PagerT m Page
createNewPage = do
    pagesCount <- getExternalState $ view pagerPagesCount
    (PagerConf handle pageSize _ _) <- ask
    let payload = B.replicate (fromIntegral pageSize - pageOverhead) 0
        page = Page (PageId pagesCount) payload NoPageId
    insertPage (view pageId page) page
    lift $ do
        hSeek handle SeekFromEnd 0
        hPut handle $ encode page
    modifyExternalState (over pagerPagesCount succ)
    return page

getCache :: Monad m => PagerT m Cache
getCache = PagerT $ gets fst

setCache :: Monad m => Cache -> PagerT m ()
setCache = PagerT . modify . first . const

getExternalState :: Monad m => (PagerState -> a) -> PagerT m a
getExternalState fn = PagerT $ gets (fn . snd)

modifyExternalState :: Monad m => (PagerState -> PagerState) -> PagerT m ()
modifyExternalState fn = PagerT $ modify $ second fn
