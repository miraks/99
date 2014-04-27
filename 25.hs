import System.Random

rndPermu :: [a] -> IO [a]
rndPermu xs = do
  gen <- getStdGen
  return $ rndPermu' xs gen

rndPermu' :: [a] -> StdGen -> [a]
rndPermu' [x] _ = [x]
rndPermu' rs gen =
  (rs !! n) : rndPermu' newRs newGen
  where
    (n, newGen) = randomR (0, length rs - 1) gen
    newRs = take n rs ++ drop (n+1) rs
