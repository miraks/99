import Data.List

pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
              in (x:first) : pack rest
