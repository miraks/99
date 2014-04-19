rotate :: [a] -> Int -> [a]
rotate xs n =
  let c = if n > 0 then n else length xs + n
  in drop c xs ++ take c xs
