import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Cache.IntMap
import System.Random

(%) = mod

lim :: Int
lim = 1000

main :: IO ()
main = do
    icache <- mkCacheIO
    vals <- forConcurrently [1..lim] $ \ n ->
        fetch icache n $ fmap sum $
            forConcurrently [m * (lim + 1) | m <- [1..n]] $ \ m ->
                fetch icache m $ do
                    print m
                    r <- randomRIO (1, 10000)
                    threadDelay r
                    return m
    print $ sum vals
