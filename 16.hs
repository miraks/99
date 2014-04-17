import Data.List

--dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
  let zips = zip xs ([0..] :: [Int])
      groups = groupBy (\(_,k) (_,m) -> k `div` n == m `div` n) zips
  in concatMap (take (n - 1) . map fst) groups
