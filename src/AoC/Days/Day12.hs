module AoC.Days.Day12 where

import AoC.Lib.Parser
import AoC.Prelude hiding (some, (\\))
import Data.Char
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set

parse :: String -> Maybe AdjMap
parse = fmap toAdjMap . parseFileWith pairP

solveA :: AdjMap -> Int
solveA = length . paths avoiderA

solveB :: AdjMap -> Int
solveB = length . paths avoiderB

type AdjMap = Map String (Set String)

paths :: (Map String Int -> Set String) -> AdjMap -> [[String]]
paths avoider m0 = go [] (visitCount m0) m0 "start"
  where
    go :: [String] -> Map String Int -> AdjMap -> String -> [[String]]
    go path seen m node
      | node == "end" = [node : path]
      | otherwise = concatMap (go (node : path) seen' m) nexts
      where
        seen' = if lower node then Map.insertWith (+) node 1 seen else seen
        nexts = (m ! node) \\ avoider seen'

avoiderA :: Map String Int -> Set String
avoiderA seen = Map.keysSet (Map.filter (> 0) seen)

avoiderB :: Map String Int -> Set String
avoiderB seen
  | Map.size (Map.filter (> 1) seen) == 0 = Set.singleton "start"
  | otherwise = avoiderA seen

visitCount :: AdjMap -> Map String Int
visitCount =
  Map.fromList
    . (\l -> zip l (repeat 0))
    . filter (\s -> s `notElem` ["start", "end"] && lower s)
    . Map.keys

lower :: String -> Bool
lower = not . any isUpper

toAdjMap :: [(String, String)] -> AdjMap
toAdjMap = foldr addEdge mempty
  where
    addEdge :: (String, String) -> AdjMap -> AdjMap
    addEdge (from, to) =
      Map.insertWith (<>) to (Set.fromList [from])
        . Map.insertWith (<>) from (Set.fromList [to])

pairP :: Parser (String, String)
pairP = do
  s1 <- someTill letterChar (string "-")
  s2 <- someTill letterChar newline
  pure (s1, s2)
