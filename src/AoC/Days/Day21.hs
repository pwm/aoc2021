module AoC.Days.Day21 where

import AoC.Lib.Grid ((<+>))
import AoC.Lib.Memo
import AoC.Lib.Parser
import AoC.Prelude
import Control.Monad.State.Strict
import Data.Hashable

parse :: String -> Maybe (Int, Int)
parse = parseMaybe gameP

solveA :: (Int, Int) -> Int
solveA = uncurry checkSum . playTill1000 . newGame

solveB :: (Int, Int) -> Int
solveB = uncurry max . playQTill21 . newGame

data Game = Game {p1 :: Player, p2 :: Player, next :: Turn}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

data Player = Player {pos :: Int, score :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

data Turn = P1 | P2
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (Hashable)

data Die = Die {val :: Int, rolls :: Int}
  deriving stock (Show, Eq, Ord, Generic)

newGame :: (Int, Int) -> Game
newGame (a, b) =
  Game
    { p1 = Player {pos = a, score = 0},
      p2 = Player {pos = b, score = 0},
      next = P1
    }

playQTill21 :: Game -> (Int, Int)
playQTill21 = memoIntMap openRec
  where
    openRec :: (Monad m) => (Game -> m (Int, Int)) -> Game -> m (Int, Int)
    openRec rec Game {p1, p2, next}
      | p1 ^. #score >= 21 = pure (1, 0)
      | p2 ^. #score >= 21 = pure (0, 1)
      | otherwise = do
        let gs
              | next == P1 = (\p -> Game p p2 P2) <$> moveQ p1
              | otherwise = (\p -> Game p1 p P1) <$> moveQ p2
        universes <- traverse rec gs
        pure $ foldl (<+>) (0, 0) universes
    moveQ :: Player -> [Player]
    moveQ p =
      let diceVals = timesM 3 d3Q (Die 0 0) ^.. each . #val
          nextPoses = (\v -> wrap 10 ((p ^. #pos) + v)) <$> diceVals
       in nextPoses <&> \pos -> p & #pos .~ pos & #score +~ pos
    d3Q :: Die -> [Die]
    d3Q (Die val rolls) =
      let d3 n = Die (val + n) (rolls + 1)
       in d3 <$> [1, 2, 3]

playTill1000 :: Game -> (Game, Die)
playTill1000 =
  flip runState (Die 0 0)
    . loopTillM (pure . reached 1000) turn
  where
    turn :: Game -> State Die Game
    turn Game {p1, p2, next}
      | next == P1 = move p1 >>= \p1' -> pure $ Game p1' p2 P2
      | otherwise = move p2 >>= \p2' -> pure $ Game p1 p2' P1
    move :: Player -> State Die Player
    move p = do
      die0 <- get
      let die1 = dSides 100 die0
          die2 = dSides 100 die1
          die3 = dSides 100 die2
      put die3
      let roll = sumOf (each . #val) [die1, die2, die3]
          nextPos = wrap 10 ((p ^. #pos) + roll)
      pure $ p & #pos .~ nextPos & #score +~ nextPos
    reached :: Int -> Game -> Bool
    reached n g = g ^. #p1 . #score >= n || g ^. #p2 . #score >= n
    dSides :: Int -> Die -> Die
    dSides sides die = die & #val %~ wrap sides . (+ 1) & #rolls +~ 1

checkSum :: Game -> Die -> Int
checkSum Game {p1, p2} die =
  let loserScore =
        if p1 ^. #score < p2 ^. #score
          then p1 ^. #score
          else p2 ^. #score
   in loserScore * die ^. #rolls

wrap :: Int -> Int -> Int
wrap bound n = (n - 1) `mod` bound + 1

gameP :: Parser (Int, Int)
gameP = do
  x <- string "Player 1 starting position: " *> intP
  y <- string "Player 2 starting position: " *> intP
  pure (x, y)
