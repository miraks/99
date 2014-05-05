primeFactors :: (Integral a) => a -> [a]
primeFactors n =
  primeFactors' n 2
  where
    primeFactors' n k
      | n == 1 = []
      | n `mod` k == 0 = k : primeFactors' (n `div` k) k
      | otherwise = primeFactors' n (k+1)
