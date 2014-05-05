isPrime :: (Integral a) => a -> Bool
isPrime n = null $ filter (==0) $ map (mod n) [2..m]
  where m = floor . sqrt $ fromIntegral n

primesR :: Int -> Int -> [Int]
primesR n k = filter isPrime [n..k]
