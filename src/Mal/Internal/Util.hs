{-| Utility functions for internal use. -}
module Mal.Internal.Util where

import qualified Data.Text as T

-- | 'pairs' takes a list and returns a list of pairs of successive elements.
--
-- >>> pairs [1 2 3 4]
-- [(1, 2), (3, 4)]
pairs :: [a] -> [(a, a)]
pairs  = go []
    where
        go rs  (x:y:zs) = go ((x, y) : rs) zs
        go rs _         = rs

-- | 'unquouteString' removes all '"' characters from a string, unless the string
-- is "\"".
unquote :: T.Text -> T.Text
unquote s@"\"" = s
unquote s      = T.filter (/= '\"') s

unquoteString :: String -> String
unquoteString = T.unpack . unquote . T.pack
