insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n =
  let (before, after) = splitAt (n-1) xs
  in before ++ x:after
