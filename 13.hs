data Encoded a = Single a | Multiple Int a deriving (Eq, Show)

encoder :: (Eq a) => [a] -> [(Int, a)]
encoder = foldr helper []
  where
    helper x [] = [(1,x)]
    helper x (y@(a,b):ys)
      | x == b = (a + 1,x):ys
      | otherwise = (1,x):y:ys

encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect = map helper . encoder
  where
    helper (1,x) = Single x
    helper (n,x) = Multiple n x
