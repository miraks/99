totient :: (Integral a) => a -> Int
totient n =
  length $ filter (coprime n) [1..n]
  where coprime a b = gcd a b == 1
