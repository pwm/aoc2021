module AoC.Days.Day03 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [[Bool]]
parse = traverse ((pure . fmap (/= 0)) <=< stringToDigits) . lines

solveA :: [[Bool]] -> Integer
solveA bbs = binToDec (gamma bbs) * binToDec (epsilon bbs)
  where
    gamma = fmap mcb . transpose
    epsilon = fmap lcb . transpose

solveB :: [[Bool]] -> Integer
solveB bbs = binToDec (oxi bbs) * binToDec (co2 bbs)
  where
    oxi = findWith (bitCriteria mcb)
    co2 = findWith (bitCriteria lcb)

mcb, lcb :: [Bool] -> Bool
mcb l = length (filter (== True) l) >= ceiling (fromIntegral @Int @Float (length l) / 2)
lcb = not . mcb

findWith :: (Int -> [[Bool]] -> [[Bool]]) -> [[Bool]] -> [Bool]
findWith finder = go 0
  where
    go :: Int -> [[Bool]] -> [Bool]
    go pos bbs
      | length bbs == 1 = head bbs
      | otherwise = go (pos + 1) (finder pos bbs)

bitCriteria :: ([Bool] -> Bool) -> Int -> [[Bool]] -> [[Bool]]
bitCriteria crit pos bbs =
  filter (\bs -> bs !! pos == crit (transpose bbs !! pos)) bbs
