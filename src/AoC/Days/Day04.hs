module AoC.Days.Day04 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: [Char] -> Maybe Game
parse s =
  MkGame
    <$> stringToIntsSepBy "," numberStrings
      <*> pure []
      <*> parseCards cardStrings
      <*> pure []
  where
    (numberStrings, cardStrings) = let ls = lines s in (head ls, tail ls)
    parseCards =
      traverse
        ( traverse
            (traverse stringToInt . filter (/= "") . splitOn " ")
            . drop 1
        )
        . chunksOf 6

solveA :: Game -> Int
solveA = checkSum . loopTill (not . null . view #bingos) turn

solveB :: Game -> Int
solveB = checkSum . loopTill (null . view #cards) turn

data Game = MkGame
  { pool :: [Int],
    drawn :: [Int],
    cards :: [Card],
    bingos :: [Card]
  }
  deriving stock (Show, Generic)

type Card = [[Int]]

turn :: Game -> Game
turn g =
  g
    & #pool %~ drop 1
    & #drawn .~ ds
    & #cards %~ (\\ bs)
    & #bingos .~ bs
  where
    ds = head (g ^. #pool) : g ^. #drawn
    bs = getBingos ds (g ^. #cards)

getBingos :: [Int] -> [Card] -> [Card]
getBingos ns =
  let bingo c = (any (\r -> null (r \\ ns)) (c <> transpose c), c)
   in map snd . filter fst . map bingo

checkSum :: Game -> Int
checkSum g
  | null (g ^. #bingos) = 0
  | otherwise = sum (concat (head (g ^. #bingos)) \\ (g ^. #drawn)) * head (g ^. #drawn)
