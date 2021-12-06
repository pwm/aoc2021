module AoC.Days.Day06 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [Int]
parse = stringToIntsSepBy "," . head . lines

solveA :: [Int] -> Int
solveA = solve 80

solveB :: [Int] -> Int
solveB = solve 256

solve :: Int -> [Int] -> Int
solve n = sum . applyTimes' n turn . counters

turn :: [Int] -> [Int]
turn l = (drop 1 l <> take 1 l) & element 6 .~ (head l + l !! 7)

counters :: [Int] -> [Int]
counters = foldr (\v -> element v %~ (+ 1)) (replicate 9 0)
