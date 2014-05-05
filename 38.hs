import Control.Arrow
import Control.Exception.Base
import Data.List
import Data.Time.Clock

totientSlow :: (Integral a) => a -> Int
totientSlow n =
  length $ filter (coprime n) [1..n]
  where coprime a b = gcd a b == 1

primeFactors :: (Integral a) => a -> [a]
primeFactors n =
  primeFactors' n 2
  where
    primeFactors' n k
      | n == 1 = []
      | n `mod` k == 0 = k : primeFactors' (n `div` k) k
      | otherwise = primeFactors' n (k+1)

primeFactorsMult :: (Integral a) => a -> [(a, Int)]
primeFactorsMult = map (head &&& length) . group . primeFactors

totientFast :: (Integral a) => a -> a
totientFast n =
  product $ map calc (primeFactorsMult n)
  where
    calc (p, m) = (p-1) * p ^ (m-1)

benchmark :: Int -> a -> IO (a, NominalDiffTime)
benchmark n f = do
  startTime <- getCurrentTime
  res <- evaluate $ last $ take n $ repeat f
  finishTime <- getCurrentTime
  return $ (res, diffUTCTime finishTime startTime)

main = do
  let times = 100000000
  putStrLn "Benchmark slow totient implementation"
  (_, timeSlow) <- benchmark times (totientSlow 10090)
  putStrLn $ "It taked " ++ (show timeSlow)
  putStrLn "Benchmark fast totient implementation"
  (_, timeFast) <- benchmark times (totientFast 10090)
  putStrLn $ "It taked " ++ (show timeFast)
