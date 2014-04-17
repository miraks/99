import Data.List

data Encoded a = Single a | Multiple Int a deriving (Eq, Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map encoder . group
  where
    encoder x
      | l > 1 = Multiple l (head x)
      | otherwise = Single (head x)
      where l = length x
