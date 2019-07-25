module Algorithm.Sorts
    ( quickSort
    ) where

import Data.List

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lhs ++ [x] ++ quickSort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (> x) xs