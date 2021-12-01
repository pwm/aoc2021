module AoC.Days.Day01 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = dists

solveB :: [Int] -> Int
solveB = dists . window3

dists :: [Int] -> Int
dists l = length $ filter (== True) $ zipWith (<) (maxBound : l) l

window3 :: [Int] -> [Int]
window3 l = zipWith3 (\a b c -> a + b + c) l (drop 1 l) (drop 2 l)
