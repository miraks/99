removeAt :: Int -> [a] -> (a,[a])
removeAt n xs =
  let (before, (x:after)) = splitAt (n-1) xs
  in (x, before ++ after)
