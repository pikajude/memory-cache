module Data.Cache (
    -- $doc
    Key,
    module Data.Cache.Map
) where

import Data.Cache.Map
import Data.Key       (Key)

-- $doc
-- A 'Data.Cache.Base.Cache' is a key-value store with threadsafe access, appending and
-- deletion.
--
-- The cache primitives in this module ('Map.fetch' and so on) are
-- re-exported from "Map", to avoid forcing investigative users to
-- accept a 'Data.Hashable.Hashable' constraint or to work only with 'Int' keys.
