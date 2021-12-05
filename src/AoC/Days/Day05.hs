module AoC.Days.Day05 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe [Segment]
parse = parseFileWith segmentP

solveA :: [Segment] -> Int
solveA = solveB . removeDiags

solveB :: [Segment] -> Int
solveB = length . filter (> 1) . Map.elems . toLineMap . toLines

data Segment = MkSegment
  { x1 :: Int,
    y1 :: Int,
    x2 :: Int,
    y2 :: Int
  }
  deriving stock (Show, Generic)

removeDiags :: [Segment] -> [Segment]
removeDiags = filter (\s -> x1 s == x2 s || y1 s == y2 s)

toLines :: [Segment] -> [(Int, Int)]
toLines = concatMap toLine
  where
    toLine :: Segment -> [(Int, Int)]
    toLine s
      | x1 s == x2 s = zip (repeat (x1 s)) (dir (y1 s) (y2 s))
      | y1 s == y2 s = zip (dir (x1 s) (x2 s)) (repeat (y1 s))
      | otherwise = zip (dir (x1 s) (x2 s)) (dir (y1 s) (y2 s))
    dir :: Int -> Int -> [Int]
    dir a b = if a < b then [a .. b] else reverse [b .. a]

toLineMap :: [(Int, Int)] -> Map (Int, Int) Int
toLineMap = foldr (\k -> Map.insertWith (+) k 1) mempty

segmentP :: Parser Segment
segmentP = do
  x1 <- intP <* symbol ","
  y1 <- intP <* symbol "->"
  x2 <- intP <* symbol ","
  y2 <- intP
  let s = MkSegment x1 y1 x2 y2
  pure s
