{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Cache.IntMap (
    Cache,
    mkCache, mkCacheIO,
    fetch, delete, insert
) where

import           Control.Concurrent.STM      (STM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl, StM)
import qualified Data.Cache.Base             as Base
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as I

-- | 'IntMap'-backed 'Cache'
type Cache = Base.Cache IntMap

mkCache :: STM (Cache v)
mkCache = Base.mkCache I.lookup I.insert I.delete
mkCacheIO :: IO (Cache v)
mkCacheIO = Base.mkCacheIO I.lookup I.insert I.delete

fetch :: (MonadBaseControl IO m, MonadIO m)
      => Cache (StM m a)
      -> Int
      -> m a
      -> m a
fetch = Base.fetch

delete :: MonadIO m => Cache a -> Int -> m ()
delete = Base.delete

insert :: (MonadBaseControl IO m, MonadIO m)
       => Cache (StM m a) -> Int -> m a -> m ()
insert = Base.insert
