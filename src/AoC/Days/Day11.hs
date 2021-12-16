module AoC.Days.Day11 where

import AoC.Lib.Grid
import AoC.Lib.SimpleParser
import AoC.Prelude
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Octopi
parse = fmap (Map.map (,True)) . parseGrid charToDigit

solveA :: Octopi -> Int
solveA =
  view _1
    . flip execState (0, False, 0)
    . timesM 100 turn

solveB :: Octopi -> Int
solveB =
  view _3
    . flip execState (0, False, 0)
    . loopTillM (const (use _2)) turn

type Octopi = GridOf (Int, Bool)

turn :: Octopi -> State (Int, Bool, Int) Octopi
turn os = do
  let os' = fixpoint flashes $ Map.map (first (+ 1)) os
      flashed' = Map.size $ Map.filter ((== False) . snd) os'
  _1 %= (+ flashed')
  _2 .= (flashed' == Map.size os)
  _3 %= (+ 1)
  pure $ zero os'

flashes :: Octopi -> Octopi
flashes os = foldr flash os (hot (canFlash os))
  where
    flash :: Pos -> Octopi -> Octopi
    flash pos os' =
      adjusts (second (const False)) [pos]
        . adjusts (first (+ 1)) (neighbours8 os' pos)
        $ os'

zero :: Octopi -> Octopi
zero os =
  Map.map (second (const True))
    . adjusts (first (const 0)) (hot os)
    $ os

canFlash :: Octopi -> Octopi
canFlash = Map.filter snd

hot :: Octopi -> [Pos]
hot = Map.keys . Map.filter ((> 9) . fst)

adjusts :: (Foldable t, Ord k) => (a -> a) -> t k -> Map k a -> Map k a
adjusts f xs m = foldr (Map.adjust f) m xs
