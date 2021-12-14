module AoC.Days.Day14 where

import AoC.Lib.Parser
import AoC.Prelude
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map

parse :: String -> Maybe ([(Char, Char)], Rules)
parse = parseMaybe inputP

solveA :: ([(Char, Char)], Rules) -> Int
solveA = solve 10

solveB :: ([(Char, Char)], Rules) -> Int
solveB = solve 40

type PairCounts = Map (Char, Char) Int

type Rules = Map (Char, Char) Char

solve :: Int -> ([(Char, Char)], Rules) -> Int
solve n (l, rules) = checkSum (snd (last l)) $ times n (step rules) (toPairCounts l)

toPairCounts :: [(Char, Char)] -> PairCounts
toPairCounts = foldr (\k m -> Map.insertWith (+) k 1 m) mempty

step :: Rules -> PairCounts -> PairCounts
step rules pairCounts =
  let f :: (Char, Char) -> Int -> State PairCounts ()
      f (a, b) n = case rules !? (a, b) of
        Nothing -> pure ()
        Just v -> modify $ Map.insertWith (+) (a, v) n . Map.insertWith (+) (v, b) n
   in execState (Map.traverseWithKey f pairCounts) mempty

checkSum :: Char -> PairCounts -> Int
checkSum lastChar pairCounts =
  let ns = Map.elems (charCount pairCounts) in maximum ns - minimum ns
  where
    charCount :: Map (Char, Char) Int -> Map Char Int
    charCount =
      Map.adjust (+ 1) lastChar
        . Map.foldrWithKey (\(a, _) -> Map.insertWith (+) a) mempty

inputP :: Parser ([(Char, Char)], Rules)
inputP = do
  s <- someTill upperChar newline <* newline
  ps <- Map.fromList <$> someTill insertionP eof
  pure (zip s (drop 1 s), ps)

insertionP :: Parser ((Char, Char), Char)
insertionP = do
  c1 <- upperChar
  c2 <- upperChar <* sc <* lexeme "->"
  c3 <- upperChar <* newline
  pure ((c1, c2), c3)
