module AoC.Days.Day09 where

import AoC.Lib.Grid
import AoC.Lib.SimpleParser
import AoC.Prelude
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map

parse :: String -> Maybe HeightMap
parse = parseGrid charToDigit

solveA :: HeightMap -> Int
solveA = sum . fmap (+ 1) . findLowPoints

solveB :: HeightMap -> Int
solveB = product . take 3 . rsort . fmap length . findBasins

type HeightMap = GridOf Int

findLowPoints :: HeightMap -> [Int]
findLowPoints hm = Map.foldrWithKey f [] hm
  where
    f :: Pos -> Int -> [Int] -> [Int]
    f pos height acc
      | height < minimum (mapMaybe (hm !?) (adj4 pos)) = height : acc
      | otherwise = acc

findBasins :: HeightMap -> [[Int]]
findBasins hm = evalState (foldM addBasin [] (Map.keys hm)) hm
  where
    addBasin :: [[Int]] -> Pos -> State HeightMap [[Int]]
    addBasin basins pos = do
      basin <- exploreBasin pos
      pure $ if null basin then basins else basin : basins

exploreBasin :: Pos -> State HeightMap [Int]
exploreBasin pos = do
  hm <- get
  case hm !? pos of
    Nothing -> pure []
    Just v -> do
      modify $ Map.delete pos
      if v == 9
        then pure []
        else do
          vs <- concat <$> traverse exploreBasin (neighbours4 hm pos)
          pure (v : vs)
