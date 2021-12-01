module AoC.Days.Day01 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = dists

solveB :: [Int] -> Int
solveB = dists . fmap sum . triplets

dists :: [Int] -> Int
dists = fst . foldl (\(acc, prev) cur -> (acc + (if prev < cur then 1 else 0), cur)) (0, maxBound)

triplets :: [Int] -> [[Int]]
triplets = reverse . go []
  where
    go c l
      | length l < 3 = c
      | otherwise = go (take 3 l : c) (drop 1 l)
