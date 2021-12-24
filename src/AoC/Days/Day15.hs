module AoC.Days.Day15 where

import AoC.Lib.Graph
import AoC.Lib.Grid hiding (step)
import AoC.Lib.SimpleParser
import AoC.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (GridOf Int)
parse = parseGrid charToDigit

solveA :: GridOf Int -> Int
solveA = solve

solveB :: GridOf Int -> Int
solveB = solve . extend

type AdjMap = Map Pos [(Pos, Int)]

solve :: GridOf Int -> Int
solve grid =
  let adjMap = toAdjMap grid
      start = fst (Map.findMin grid)
      dest = fst (Map.findMax grid)
   in snd $ last $ dijkstra (step adjMap) (== dest) start

step :: AdjMap -> Pos -> [(Pos, Int)]
step adjMap node = fromMaybe [] (adjMap !? node)

toAdjMap :: GridOf Int -> AdjMap
toAdjMap grid =
  Map.foldrWithKey (\p _ -> Map.insert p (mapMaybe lkps $ adj4 p)) mempty grid
  where
    lkps :: Pos -> Maybe (Pos, Int)
    lkps k = case grid !? k of
      Nothing -> Nothing
      Just v -> Just (k, v)

extend :: GridOf Int -> GridOf Int
extend g0 = Map.unions $ map (\f -> f g0) (toFs $ (,) <$> [0 .. 4] <*> [0 .. 4])
  where
    toFs :: [(Int, Int)] -> [GridOf Int -> GridOf Int]
    toFs = fmap (\(x, y) -> times x (copyTo toR) . times y (copyTo toD))
    copyTo :: (GridOf Int -> Pos -> Int -> (Pos, Int)) -> GridOf Int -> GridOf Int
    copyTo copyDir g = Map.foldrWithKey (\p w -> uncurry Map.insert (copyDir g p w)) mempty g
    toR, toD :: GridOf Int -> Pos -> Int -> (Pos, Int)
    toR g p i = (p <+> (0, sqrtInt (Map.size g)), if i == 9 then 1 else i + 1)
    toD g p i = (p <+> (sqrtInt (Map.size g), 0), if i == 9 then 1 else i + 1)
