split :: [a] -> Int -> ([a], [a])
split xs n =
  mapTuple (map fst) $ span ((< n) . snd) $ zip xs [0..]
  where
    mapTuple f (a1, a2) = (f a1, f a2)
