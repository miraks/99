isPrime :: (Integral a) => a -> Bool
isPrime n = null $ filter (==0) $ map (mod n) [2..m]
  where m = floor . sqrt $ fromIntegral n

primes :: [Int]
primes = filter isPrime [2..]

goldbach :: Int -> (Int, Int)
goldbach n =
  goldbach' primes primes
  where
    goldbach' (x:xs) (y:ys)
      | x + y < n = goldbach' (x:xs) ys
      | x + y > n = goldbach' xs xs
      | otherwise = (x, y)

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n k = map goldbach $ filter even [n..k]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' n k l = filter (\(a, _) -> a > l) $ goldbachList n k
