slice :: [a] -> Int -> Int -> [a]
slice xs n k =
  take (k-n+1) $ drop (n-1) xs
