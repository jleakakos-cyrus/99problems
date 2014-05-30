import System.Random (newStdGen, randomRs)
import Control.Applicative ((<*>))
import Data.List (nub, permutations, sortBy, groupBy)

-- Problem 21 - Insert an element at a given position into a list
insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take (i-1) xs ++ [x] ++ drop (i-1) xs

-- Problem 22 - Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range start end = [start..end]

rangeIterate :: Int -> Int -> [Int]
rangeIterate start end = take (end - start + 1) $ iterate (+1) start

-- Problem 23 - Extract a given number of randomly selected elements from a list
randomSelect :: Show a => Int -> [a] -> IO ()
randomSelect n xs = do
  gen <- newStdGen
  let ri = randomRs (0,length xs - 1) gen :: [Int]
  let rs = take n $ nub ri :: [Int]
  let slices = map (flip (!!)) rs :: [[a] -> a]
  print $ slices <*> [xs]
  return ()

-- Problem 24 - Draw N different numbers from the set 1..M
randomDraw :: Int -> Int -> IO ()
randomDraw count upperBound = do
  gen <- newStdGen
  let rs = take count $ randomRs (1, upperBound) gen :: [Int]
  print rs
  return ()

-- Problem 25 - Generate a random permutation of the elements of the list
randomPermutation :: Show a => [a] -> IO ()
randomPermutation xs = do
  gen <- newStdGen
  let perms = permutations xs
  let r = (head $ randomRs (0, length perms - 1) gen) :: Int
  let perm = perms !! r
  print perm
  return ()

-- Problem 26 - Generate the combinations of K distinct objects chosen from the N elements of a list
-- too lazy to do this one because the extra description I didn't copy here is stupid

-- Problem 27 - same as problem 26, I'm too lazy to actually do it because the description is stupid

-- Problem 28 - Sorting a list of lists according to the length of sublists
sublistLengthSort :: [[a]] -> [[a]]
sublistLengthSort xs = sortBy (\a b -> compare (length a) (length b)) xs

sublistLengthFrequencySort :: [[a]] -> [[a]]
sublistLengthFrequencySort xs = concat $ sortBy compareLength $ groupBy groupLength $ sortBy compareLength xs
  where compareLength = (\a b -> compare (length a) (length b))
        groupLength = (\a b -> length a == length b)
