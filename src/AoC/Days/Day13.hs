module AoC.Days.Day13 where

import Advent.OCR (parseLetters)
import AoC.Lib.Parser
import AoC.Prelude hiding (fold, oneOf, some)
import Data.Set qualified as Set

parse :: String -> Maybe (Paper, [Axis])
parse = parseMaybe taskP

solveA :: (Paper, [Axis]) -> Int
solveA (p, as) = Set.size $ foldOne p (head as)

solveB :: (Paper, [Axis]) -> String
solveB (p, as) = fromMaybe "" (parseLetters (foldAll p as))

data Axis = X Int | Y Int
  deriving stock (Show)

type Paper = Set (Int, Int)

foldAll :: Paper -> [Axis] -> Paper
foldAll = foldl' foldOne

foldOne :: Paper -> Axis -> Paper
foldOne paper axis =
  let (p1, p2) = cut axis paper
   in Set.union p1 (mirror axis p2)

cut :: Axis -> Paper -> (Paper, Paper)
cut (X n) = Set.partition ((< n) . fst)
cut (Y n) = Set.partition ((< n) . snd)

mirror :: Axis -> Paper -> Paper
mirror (X n) = Set.foldr (\(x, y) -> Set.insert (n - (x - n), y)) mempty
mirror (Y n) = Set.foldr (\(x, y) -> Set.insert (x, n - (y - n))) mempty

taskP :: Parser (Paper, [Axis])
taskP = do
  paper <- foldr Set.insert mempty <$> some pairP
  axes <- some axisP
  pure (paper, axes)

pairP :: Parser (Int, Int)
pairP = do
  n1 <- intP <* string ","
  n2 <- intP
  pure (n1, n2)

axisP :: Parser Axis
axisP = do
  c <- string "fold along " *> (char 'x' <|> char 'y') <* char '='
  n <- intP
  let parseAxis :: Char -> Maybe Axis
      parseAxis =
        \case
          'x' -> Just $ X n
          'y' -> Just $ Y n
          _ -> Nothing
  maybeToP parseAxis c
