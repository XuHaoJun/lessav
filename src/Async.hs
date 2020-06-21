module Async
    ( 
      mapPool
    ) where

import Control.Concurrent.MSem
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import qualified Data.Traversable as T

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs
