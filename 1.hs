myLast :: [a] -> a
myLast [] = error "list is empty"
myLast [x] = x
myLast (x:xs) = myLast xs
