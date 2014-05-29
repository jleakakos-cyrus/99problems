-- Problem 1 - Find the last element of a list
myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- Problem 2 - Find the last but one element of a list
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem 3 - Find the kth element of a list; the first element is 1
elementAt :: [a] -> Int -> a
elementAt xs i = elementAt' xs i 1

elementAt' :: [a] -> Int -> Int -> a
elementAt' (y:ys) i ci
  | ci == i = y
  | otherwise = elementAt' ys i (ci+1)

-- Problem 4 - Find the number of elements in the list
myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0

-- Problem 5 - Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (\acc v -> (v:acc)) []

-- Problem 6 - Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7 - Flatten a nested list structure
-- too lazy to do - haskell doesn't have default arbitrary nesting for lists

-- Problem 8 - Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress = foldr foldingFunction []
  where foldingFunction v [] = [v]
        foldingFunction v (a:as) = if v == a then (a:as) else (v:a:as)

-- Problem 9 - Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack = foldr foldingFunction []
  where foldingFunction v [] = [[v]]
        foldingFunction v (a:as) = if v == head a then ((v:a):as) else ([v]:(a:as))

-- Problem 10 - Run-length encoding of a list
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr foldingFunction []
  where foldingFunction v [] = [(v, 1)]
        foldingFunction v ((a, n):as) = if v == a then ((a, n+1):as) else ((v,1):((a, n):as))
