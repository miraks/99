import Control.Monad

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom xs = xs == reverse xs
