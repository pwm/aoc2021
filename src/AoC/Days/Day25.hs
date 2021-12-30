module AoC.Days.Day25 where

import AoC.Lib.Grid
import AoC.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA = run

solveB :: Grid -> ()
solveB _ = ()

data Cell = East | South | Empty
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

type Grid = GridOf Cell

run :: Grid -> Int
run = go 1
  where
    go :: Int -> Grid -> Int
    go c g
      | g == step1 g = c
      | otherwise = go (c + 1) (step1 g)

step1 :: Grid -> Grid
step1 g0 =
  let g1 = foldr update g0 (eMoves g0)
   in foldr update g1 (sMoves g1)

update :: (Pos, Pos, Cell) -> Grid -> Grid
update (from, to, cell) = Map.insert to cell . Map.insert from Empty

eMoves, sMoves :: Grid -> [(Pos, Pos, Cell)]
eMoves g = mapMaybe (eMove g) . Map.keys . Map.filter (== East) $ g
sMoves g = mapMaybe (sMove g) . Map.keys . Map.filter (== South) $ g

eMove, sMove :: Grid -> Pos -> Maybe (Pos, Pos, Cell)
eMove g p@(r, c) = case g !? (r, c + 1) of
  Just Empty -> Just (p, (r, c + 1), East)
  Just _ -> Nothing
  Nothing -> case g !? (r, 0) of
    Just Empty -> Just (p, (r, 0), East)
    _ -> Nothing
sMove g p@(r, c) = case g !? (r + 1, c) of
  Just Empty -> Just (p, (r + 1, c), South)
  Just _ -> Nothing
  Nothing -> case g !? (0, c) of
    Just Empty -> Just (p, (0, c), South)
    _ -> Nothing

parseCell :: Char -> Maybe Cell
parseCell = \case
  '>' -> Just East
  'v' -> Just South
  '.' -> Just Empty
  _ -> Nothing
