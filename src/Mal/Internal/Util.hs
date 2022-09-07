module Mal.Internal.Util where

pairs :: [a] -> [(a, a)]
pairs  = go []
    where
        go rs  (x:y:zs) = go ((x, y) : rs) zs
        go rs _         = rs
