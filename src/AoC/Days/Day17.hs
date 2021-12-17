module AoC.Days.Day17 where

import AoC.Lib.Grid hiding (step)
import AoC.Lib.Parser
import AoC.Prelude
import Data.Set qualified as Set

parse :: String -> Maybe ((Int, Int), (Int, Int))
parse = parseMaybe targetP

solveA :: ((Int, Int), (Int, Int)) -> Int
solveA = maximum . fmap snd . concat . solve

solveB :: ((Int, Int), (Int, Int)) -> Int
solveB = length . solve

newtype Velocity = V Pos
  deriving stock (Show)

solve :: ((Int, Int), (Int, Int)) -> [[Pos]]
solve ((xMin, xMax), (yMin, yMax)) =
  filter (not . null) $ aim target (xMax, yMin) . V <$> velocities
  where
    target = Set.fromList $ mkRect xMin xMax yMin yMax
    velocities = (,) <$> [1 .. xMax] <*> [yMin .. negate yMin]

aim :: Set Pos -> Pos -> Velocity -> [Pos]
aim target (xMax, yMin) v0 =
  let (res, ps) = go (v0, (0, 0))
   in if res then ps else []
  where
    go :: (Velocity, Pos) -> (Bool, [Pos])
    go (v, p)
      | isHit p = (True, [])
      | isPast p = (False, [])
      | otherwise = second (p :) (go (step (v, p)))
    isHit p = Set.member p target
    isPast (x, y) = x > xMax || y < yMin

step :: (Velocity, Pos) -> (Velocity, Pos)
step (V (vx, vy), (x, y)) = (V (drag vx, vy -1), (x + vx, y + vy))

drag :: Int -> Int
drag x
  | x > 0 = x - 1
  | x < 0 = x + 1
  | otherwise = 0

targetP :: Parser ((Int, Int), (Int, Int))
targetP = do
  xMin <- string "target area: x=" *> intP <* string ".."
  xMax <- intP <* string ", y="
  yMin <- signedIntP <* string ".."
  yMax <- signedIntP
  pure ((xMin, xMax), (yMin, yMax))
