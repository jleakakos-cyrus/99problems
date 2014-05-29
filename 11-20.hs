import Data.List (group)

-- Problem 11 - Modified run-length encoding
data ListItem a = Single a | Multiple Int a deriving Show
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map toListItem . group

toListItem :: [a] -> ListItem a
toListItem (x:[]) = Single x
toListItem xs@(x:_) = Multiple (length xs) x

-- Problem 12 -- Decode a run-length encoded list
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap fromListItem

fromListItem :: ListItem a -> [a]
fromListItem (Single x) = [x]
fromListItem (Multiple n x) = replicate n x

-- Problem 13 - Run-length encoding of a list directly
-- again, too lazy to do this one

-- Problem 14 - Duplicate the elements of a list
duplicate :: [a] -> [a]
duplicate = foldr (\v acc -> (v:v:acc)) []

-- Problem 15 - Replicate the elements of a list a given number of times
myReplicate :: [a] -> Int -> [a]
myReplicate xs n = concatMap (replicate n) xs

-- Problem 16 - Drop every Nth element from a list
dropEvery :: Int -> [a] -> [a]
dropEvery n xs = map snd $ filter (\(i,_) -> i `mod` n /= 0) (zip [1..] xs)

dropEveryIterative :: Int -> [a] -> [a]
dropEveryIterative n xs = dropEveryIterative' n 1 xs

dropEveryIterative' :: Int -> Int -> [a] -> [a]
dropEveryIterative' _ _ [] = []
dropEveryIterative' n i (x:xs)
  | i `mod` n == 0 = dropEveryIterative' n (i+1) (xs)
  | otherwise = x:dropEveryIterative' n (i+1) (xs)

-- Problem 17 - Split a list into two parts; the length of the first part is given
mySplit :: [a] -> Int -> [[a]]
mySplit xs n = [take n xs, drop n xs]

-- Problem 18 - Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs start end = take (end - start+1) $ drop (start-1) xs

-- Problem 19 - Rotate a list n places to the left
rotate :: Int -> [a] -> [a]
rotate n xs 
  | n >= 0 = drop n xs ++ take n xs
  | n < 0 = drop (length xs + n) xs ++ take (length xs + n) xs

-- Problem 20 - Remove the kth element from a list
removeAt k xs = take (k-1) xs ++ drop k xs
