{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Base functions for constructing and modifying 'Cache's.
--
-- The other modules in this package ("Data.Cache.Map" and so on) re-export
-- specialized versions of the functions in this package, with
-- nicer-looking type signatures.
module Data.Cache.Base (
    Cache,
    mkCache, mkCacheIO,
    fetch, delete, insert
) where

import Control.Applicative
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control     (MonadBaseControl, StM)
import Data.Key
import Data.Maybe

-- | An opaque type describing an in-memory cache.
data Cache container value =
        Cache { cache    :: TVar (container (Async value))
              , keyCache :: TVar (container ())
              , retrieve :: forall v. Key container -> container v -> Maybe v
              , insert_  :: forall v. Key container -> v -> container v -> container v
              , remove   :: forall v. Key container -> container v -> container v
              }

-- | @'mkCache' f g h@ creates a new 'Cache', where @f@ is the lookup function,
-- @g@ is the insertion function, and @h@ is the deletion function.
--
-- Generally, the data structure that backs the 'Cache' can be inferred from
-- the types of these three functions, e.g.
--
-- >>> :t mkCache IntMap.lookup IntMap.insert IntMap.delete
-- STM (Cache IntMap v)
mkCache :: (Monoid (cont (Async a)), Monoid (cont ()))
        => (forall v. Key cont -> cont v -> Maybe v)     -- ^ Retrieval function
        -> (forall v. Key cont -> v -> cont v -> cont v) -- ^ Insertion function
        -> (forall v. Key cont -> cont v -> cont v)      -- ^ Deletion function
        -> STM (Cache cont a)
mkCache ret ins del = do
    tv <- newTVar mempty
    sv <- newTVar mempty
    return Cache { cache = tv, keyCache = sv
                 , retrieve = ret
                 , insert_ = ins
                 , remove = del
                 }

-- | Identical to 'mkCache', but can be used inside 'System.IO.Unsafe.unsafePerformIO' to
-- allow creating top-level caches.
mkCacheIO :: (Monoid (cont ()), Monoid (cont (Async a)))
          => (forall v. Key cont -> cont v -> Maybe v)     -- ^ Retrieval function
          -> (forall v. Key cont -> v -> cont v -> cont v) -- ^ Insertion function
          -> (forall v. Key cont -> cont v -> cont v)      -- ^ Deletion function
          -> IO (Cache cont a)
mkCacheIO ret ins del = do
    tv <- newTVarIO mempty
    sv <- newTVarIO mempty
    return Cache { cache = tv, keyCache = sv
                 , retrieve = ret
                 , insert_ = ins
                 , remove = del
                 }

-- | @'fetch' k expensiveAction cache@ fetches the value associated with
-- @k@ from the cache, or inserts and returns the result of
-- @expensiveAction@ if the key is not set.
fetch :: (MonadBaseControl IO m, MonadIO m)
      => Cache t (StM m a) -- ^ Input cache
      -> Key t             -- ^ Cache key
      -> m a               -- ^ Action that seeds the cache
      -> m a
fetch c@Cache{..} k action = liftIO (atomically (getAsync <|> Nothing <$ lockKey c k))
    >>= maybe
            (do act <- async action
                liftIO $ atomically $ insertAndDisengageLock act
                wait act)
            wait
    where
        getAsync = do
            t <- retrieve k <$> readTVar cache
            check (isJust t)
            return t
        insertAndDisengageLock as = do
            modifyTVar' cache (insert_ k as)
            modifyTVar' keyCache (remove k)

-- | @'insert' k expensiveAction cache@ inserts an 'Async' representing
-- @expensiveAction@ into the cache, and returns immediately.
--
-- Any existing 'Async' at the same location will be canceled.
insert :: (MonadIO m, MonadBaseControl IO m)
       => Cache t (StM m a) -> Key t -> m a -> m ()
insert c@Cache{..} k action = do
    existing <- liftIO $ atomically $
        getAsync <|> Nothing <$ lockKey c k
    forM_ existing cancel
    act <- async action
    liftIO $ atomically $ insertAndDisengageLock act
    where
        getAsync = do
            t <- retrieve k <$> readTVar cache
            check (isJust t)
            return t
        insertAndDisengageLock as = do
            modifyTVar' cache (insert_ k as)
            modifyTVar' keyCache (remove k)

-- | @'delete' k cache@ removes the value associated with @k@ from @cache@.
-- If the key does not exist, this operation does nothing.
delete :: MonadIO m
       => Cache t a -> Key t -> m ()
delete c@Cache{..} k = liftIO $ atomically $ do
    lockKey c k
    modifyTVar' cache (remove k)
    modifyTVar' keyCache (remove k)

lockKey :: Cache t a -> Key t -> STM ()
lockKey Cache{..} k =
    isJust . retrieve k <$> readTVar keyCache
    >>= check . not
    >> modifyTVar' keyCache (insert_ k ())
