import System.Random (newStdGen, randomRs)
import Control.Applicative ((<*>))
import Data.List (nub)

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
