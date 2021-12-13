{-# LANGUAGE TupleSections #-}

module AoC.Days.Day11 where

import AoC.Lib.Grid
import AoC.Lib.Load
import AoC.Lib.SimpleParser
import AoC.Prelude
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Map.Strict qualified as Map
import Data.Maybe

{-
s <- load 11
i = fromJust $ parse s
solveA i
-}

parse :: String -> Maybe Octopi
parse = fmap (Map.map (,True)) . parseGrid charToDigit

solveA :: Octopi -> Int
solveA = fst . snd . turns 100

solveB :: Octopi -> Int
solveB = allFlash

--

type Octopi = GridOf (Int, Bool)

turns :: Int -> Octopi -> (Octopi, (Int, Bool))
turns n = flip runState (0, False) . timesM n turn

allFlash :: Octopi -> Int
allFlash = go 0 (0, False)
  where
    go :: Int -> (Int, Bool) -> Octopi -> Int
    go n s os
      | snd s = n
      | otherwise = let (os', s') = runState (turn os) s in go (n + 1) s' os'

turn :: Octopi -> State (Int, Bool) Octopi
turn os = do
  flashed <- gets fst
  let os1 = inc os
      os2 = fixpoint booms os1
      flashed' = Map.size $ Map.filter ((== False) . snd) os2
      os3 = zero os2
      allFlashed' = flashed' == Map.size os
  put (flashed + flashed', allFlashed')
  pure os3

inc :: Octopi -> Octopi
inc = Map.map (first (+ 1))

booms :: Octopi -> Octopi
booms os = foldr boom os (hot (canExplode os))

boom :: Pos -> Octopi -> Octopi
boom pos os = adjusts (second (const False)) [pos] os1
  where
    os1 = adjusts (first (+ 1)) (neighbours8 os pos) os

zero :: Octopi -> Octopi
zero os = os2
  where
    os1 = adjusts (first (const 0)) (hot os) os
    os2 = Map.map (second (const True)) os1

canExplode :: Octopi -> Octopi
canExplode = Map.filter snd

hot :: Octopi -> [Pos]
hot = Map.keys . Map.filter ((> 9) . fst)

adjusts :: (Foldable t, Ord k) => (a -> a) -> t k -> Map k a -> Map k a
adjusts f xs m = foldr (Map.adjust f) m xs

--

s0, s1 :: String
s0 = "11111\n19991\n19191\n19991\n11111\n"
s1 = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n"

i0, i1 :: Octopi
i0 = Map.fromList [((0, 0), (1, True)), ((0, 1), (1, True)), ((0, 2), (1, True)), ((0, 3), (1, True)), ((0, 4), (1, True)), ((1, 0), (1, True)), ((1, 1), (9, True)), ((1, 2), (9, True)), ((1, 3), (9, True)), ((1, 4), (1, True)), ((2, 0), (1, True)), ((2, 1), (9, True)), ((2, 2), (1, True)), ((2, 3), (9, True)), ((2, 4), (1, True)), ((3, 0), (1, True)), ((3, 1), (9, True)), ((3, 2), (9, True)), ((3, 3), (9, True)), ((3, 4), (1, True)), ((4, 0), (1, True)), ((4, 1), (1, True)), ((4, 2), (1, True)), ((4, 3), (1, True)), ((4, 4), (1, True))]
i1 = Map.fromList [((0, 0), (5, True)), ((0, 1), (4, True)), ((0, 2), (8, True)), ((0, 3), (3, True)), ((0, 4), (1, True)), ((0, 5), (4, True)), ((0, 6), (3, True)), ((0, 7), (2, True)), ((0, 8), (2, True)), ((0, 9), (3, True)), ((1, 0), (2, True)), ((1, 1), (7, True)), ((1, 2), (4, True)), ((1, 3), (5, True)), ((1, 4), (8, True)), ((1, 5), (5, True)), ((1, 6), (4, True)), ((1, 7), (7, True)), ((1, 8), (1, True)), ((1, 9), (1, True)), ((2, 0), (5, True)), ((2, 1), (2, True)), ((2, 2), (6, True)), ((2, 3), (4, True)), ((2, 4), (5, True)), ((2, 5), (5, True)), ((2, 6), (6, True)), ((2, 7), (1, True)), ((2, 8), (7, True)), ((2, 9), (3, True)), ((3, 0), (6, True)), ((3, 1), (1, True)), ((3, 2), (4, True)), ((3, 3), (1, True)), ((3, 4), (3, True)), ((3, 5), (3, True)), ((3, 6), (6, True)), ((3, 7), (1, True)), ((3, 8), (4, True)), ((3, 9), (6, True)), ((4, 0), (6, True)), ((4, 1), (3, True)), ((4, 2), (5, True)), ((4, 3), (7, True)), ((4, 4), (3, True)), ((4, 5), (8, True)), ((4, 6), (5, True)), ((4, 7), (4, True)), ((4, 8), (7, True)), ((4, 9), (8, True)), ((5, 0), (4, True)), ((5, 1), (1, True)), ((5, 2), (6, True)), ((5, 3), (7, True)), ((5, 4), (5, True)), ((5, 5), (2, True)), ((5, 6), (4, True)), ((5, 7), (6, True)), ((5, 8), (4, True)), ((5, 9), (5, True)), ((6, 0), (2, True)), ((6, 1), (1, True)), ((6, 2), (7, True)), ((6, 3), (6, True)), ((6, 4), (8, True)), ((6, 5), (4, True)), ((6, 6), (1, True)), ((6, 7), (7, True)), ((6, 8), (2, True)), ((6, 9), (1, True)), ((7, 0), (6, True)), ((7, 1), (8, True)), ((7, 2), (8, True)), ((7, 3), (2, True)), ((7, 4), (8, True)), ((7, 5), (8, True)), ((7, 6), (1, True)), ((7, 7), (1, True)), ((7, 8), (3, True)), ((7, 9), (4, True)), ((8, 0), (4, True)), ((8, 1), (8, True)), ((8, 2), (4, True)), ((8, 3), (6, True)), ((8, 4), (8, True)), ((8, 5), (4, True)), ((8, 6), (8, True)), ((8, 7), (5, True)), ((8, 8), (5, True)), ((8, 9), (4, True)), ((9, 0), (5, True)), ((9, 1), (2, True)), ((9, 2), (8, True)), ((9, 3), (3, True)), ((9, 4), (7, True)), ((9, 5), (5, True)), ((9, 6), (1, True)), ((9, 7), (5, True)), ((9, 8), (2, True)), ((9, 9), (6, True))]

-- p :: Octopi -> IO ()
-- p = putStrLn . pp

-- pp :: Octopi -> String
-- pp = printGrid (head . show . fst)
