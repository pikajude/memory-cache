module Data.Cache (
    -- $doc
    module Data.Cache.Map
) where

import Data.Cache.Map

-- $doc
-- A 'Data.Cache.Base.Cache' is a key-value store with threadsafe access, appending and
-- deletion.
--
-- This module simply re-exports "Data.Cache.Map", to avoid forcing
-- a 'Data.Hashable.Hashable' constraint or 'Int' keys onto investigative
-- users. Once you're up-and-running, it is recommended to switch to one of
-- the specialized containers, as they both have better performance than
-- 'Data.Map.Map'.
