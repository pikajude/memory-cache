{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Cache.Map (
    Cache,
    mkCache, mkCacheIO,
    fetch, delete, insert
) where

import           Control.Concurrent.STM      (STM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl, StM)
import qualified Data.Cache.Base             as Base
import           Data.Map                    (Map)
import qualified Data.Map                    as M

-- | 'Map'-backed 'Cache'
type Cache k = Base.Cache (Map k)

mkCache :: Ord k => STM (Cache k v)
mkCache = Base.mkCache M.lookup M.insert M.delete
mkCacheIO :: Ord k => IO (Cache k v)
mkCacheIO = Base.mkCacheIO M.lookup M.insert M.delete

fetch :: (MonadBaseControl IO m, MonadIO m)
      => Cache k (StM m a) -> k -> m a -> m a
fetch = Base.fetch

delete :: MonadIO m => Cache k a -> k -> m ()
delete = Base.delete

insert :: (MonadBaseControl IO m, MonadIO m)
       => Cache k (StM m a) -> k -> m a -> m ()
insert = Base.insert
