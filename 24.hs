import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n k = do
  gen <- getStdGen
  return $ takeFrom n [] gen
  where
    takeFrom 0 xs gen = xs
    takeFrom m xs gen
      | x `elem` xs = takeFrom m xs newGen
      | otherwise = takeFrom (m-1) (x:xs) newGen
      where
        (x, newGen) = randomR (1, k) gen
