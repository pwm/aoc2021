module AoC.Lib.SimpleParser where

import AoC.Prelude

-- "123456" -> Just 123456
stringToInt :: String -> Maybe Int
stringToInt s =
  let (i, s') = head (reads @Int s)
   in if null s' then Just i else Nothing

-- "123\n456" -> Just [123, 456]
stringToInts :: String -> Maybe [Int]
stringToInts = traverse stringToInt . lines

-- "12,34,56" -> Just [12,34,56]
-- "12-34" -> Just [12,34]
stringToIntsSepBy :: String -> String -> Maybe [Int]
stringToIntsSepBy sep = traverse stringToInt . splitOn sep

-- "123456" -> Just [1, 2, 3, 4, 5, 6]
stringToDigits :: String -> Maybe [Int]
stringToDigits s = if length xs == length s then Just xs else Nothing
  where
    xs = concatMap (fmap fst . (\c -> reads @Int [c])) s

-- '1' -> Just 1
charToDigit :: Char -> Maybe Int
charToDigit c = case reads @Int [c] of
  [(n, "")] -> Just n
  _ -> Nothing

-- (-123456) -> [1, 2, 3, 4, 5, 6]
-- 0 -> [0]
integerToDigits :: Integer -> [Int]
integerToDigits i = if i == 0 then [0] else reverse $ unfoldr go (abs i)
  where
    go :: Integer -> Maybe (Int, Integer)
    go n = if n == 0 then Nothing else Just (fromInteger @Int (n `mod` 10), n `div` 10)

-- [1, 2, 3, 4, 5, 6] -> Just 123456
-- [] -> Nothing
digitsToInteger :: [Int] -> Maybe Integer
digitsToInteger [] = Nothing
digitsToInteger xs = Just $ foldl (\i d -> i * 10 + toInteger d) 0 xs

intToDigits :: Int -> [Int]
intToDigits = integerToDigits . fromIntegral

digitsToInt :: [Int] -> Maybe Int
digitsToInt = fmap fromIntegral . digitsToInteger
