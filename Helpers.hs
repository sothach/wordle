module Helpers where

import Data.Set (Set, union)

mergeSets :: Ord a => [(Set a, Set a)] -> [Set a]
mergeSets setsList = map(\(a,b) -> union a b) setsList

set2string :: Foldable t => t Char -> [Char]
set2string set = foldl (\acc c -> acc ++ [c]) "" set