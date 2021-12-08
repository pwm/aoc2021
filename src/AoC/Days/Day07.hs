module AoC.Days.Day07 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [Int]
parse = stringToIntsSepBy "," . head . lines

solveA :: [Int] -> Int
solveA = minDist id

solveB :: [Int] -> Int
solveB = minDist (\d -> d * (d + 1) `div` 2)

minDist :: (Int -> Int) -> [Int] -> Int
minDist f ns =
  minimum
    [ sum [f $ abs (n - d) | d <- ns]
      | n <- [minimum ns .. maximum ns]
    ]
