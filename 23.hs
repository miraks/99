import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  gen <- getStdGen
  return $ extractor gen
  where
    extractor gen =
      take n $ map (xs !!) $ randomRs (0, l-1) gen
      where l = length xs
