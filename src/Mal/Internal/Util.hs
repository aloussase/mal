{-| Utility functions for internal use. -}
module Mal.Internal.Util where

-- | 'pairs' takes a list and returns a list of pairs of successive elements.
--
-- >>> pairs [1 2 3 4]
-- [(1, 2), (3, 4)]
pairs :: [a] -> [(a, a)]
pairs  = go []
    where
        go rs  (x:y:zs) = go ((x, y) : rs) zs
        go rs _         = rs

