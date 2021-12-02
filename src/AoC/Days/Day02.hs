module AoC.Days.Day02 where

import AoC.Lib.Parser
import AoC.Prelude hiding (Down, oneOf)

parse :: String -> Maybe [Move]
parse = parseFileWith moveP

solveA :: [Move] -> Int
solveA = check . foldl' moveA pos0

solveB :: [Move] -> Int
solveB = check . foldl' moveB pos0

check :: Pos -> Int
check (MkPos _ h d) = h * d

data Move
  = Down Int
  | Up Int
  | Forward Int
  deriving stock (Show)

data Pos = MkPos {aim :: Int, hor :: Int, depth :: Int}
  deriving stock (Show)

pos0 :: Pos
pos0 = MkPos 0 0 0

moveA :: Pos -> Move -> Pos
moveA (MkPos a h d) (Down n) = MkPos a h (d + n)
moveA (MkPos a h d) (Up n) = MkPos a h (d - n)
moveA (MkPos a h d) (Forward n) = MkPos a (h + n) d

moveB :: Pos -> Move -> Pos
moveB (MkPos a h d) (Down n) = MkPos (a + n) h d
moveB (MkPos a h d) (Up n) = MkPos (a - n) h d
moveB (MkPos a h d) (Forward n) = MkPos a (h + n) (d + a * n)

moveP :: Parser Move
moveP = do
  dir <- choice $ lexeme . string <$> ["down", "up", "forward"]
  x <- intP
  pure $
    if
        | dir == "down" -> Down x
        | dir == "up" -> Up x
        | otherwise -> Forward x
