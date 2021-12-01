module AoC.Days.Day01 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = dists

solveB :: [Int] -> Int
solveB = dists . fmap sum . window 3

dists :: [Int] -> Int
dists = fst . foldl (\(acc, prev) cur -> (acc + (if prev < cur then 1 else 0), cur)) (0, maxBound)

window :: Int -> [Int] -> [[Int]]
window n0 = reverse . go n0 []
  where
    go n c l
      | length l < n = c
      | otherwise = go n (take n l : c) (drop 1 l)
