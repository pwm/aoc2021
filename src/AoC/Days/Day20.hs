module AoC.Days.Day20 where

import AoC.Lib.Grid
import AoC.Prelude
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Prog
parse s =
  Just $
    Prog
      { algo = IntMap.fromList . zip [0 ..] . map (== '#') $ head ls,
        img = grid <> expanded 0 (length imgLs - 1) False,
        inf = IntMap.fromList $ zip [0 ..] $ replicate 8 False,
        low = 0,
        high = length imgLs - 1
      }
  where
    ls = lines s
    imgLs = drop 2 ls
    grid = fromMaybe mempty $ parseGrid parseCell (unlines imgLs)
    parseCell :: Char -> Maybe Bool
    parseCell = \case
      '#' -> Just True
      '.' -> Just False
      _ -> Nothing

solveA :: Prog -> Int
solveA = solve 2

solveB :: Prog -> Int
solveB = solve 50

solve :: Int -> Prog -> Int
solve n = Map.size . Map.filter (== True) . img . times n enhance

data Prog = Prog
  { algo :: IntMap Bool,
    img :: GridOf Bool,
    inf :: IntMap Bool,
    low :: Int,
    high :: Int
  }
  deriving stock (Show)

enhance :: Prog -> Prog
enhance Prog {algo, img, inf, low, high} =
  Prog algo img' inf' (low - 1) (high + 1)
  where
    newMap = expanded (low - 1) (high + 1) False
    img' = Map.mapWithKey (\p _ -> algoLkp (imgLkp <$> sort (adj9 p))) newMap
    inf' = IntMap.fromList $ zip [0 ..] $ replicate 8 (algoLkp (IntMap.elems inf))
    imgLkp :: Pos -> Bool
    imgLkp = fromMaybe (inf IntMap.! 4) . (img !?)
    algoLkp :: [Bool] -> Bool
    algoLkp n = algo ! b2i n

expanded :: Int -> Int -> Bool -> GridOf Bool
expanded l u b =
  let rect = mkRect (l - 1) (u + 1) (l - 1) (u + 1)
   in Map.fromList $ zip rect (repeat b)

b2i :: [Bool] -> Int
b2i = fromIntegral . binToDec
