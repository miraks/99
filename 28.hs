import Data.List
import Data.Function

lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

lfsort :: [[a]] -> [[a]]
lfsort xs =
  sortBy (compare `on` lenFr) xs
  where
    lenFrs = map length xs
    lenFr x = length $ filter (== length x) lenFrs
