module AoC.Days.Day08 where

import AoC.Lib.Parser
import AoC.Prelude hiding (oneOf, set, some)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe [([Digit], [Digit])]
parse = parseFileWith lineP

solveA :: [([Digit], [Digit])] -> Int
solveA = length . filter (`elem` [2, 3, 4, 7]) . concatMap (fmap length . snd)

solveB :: [([Digit], [Digit])] -> Int
solveB = sum . fmap solveRow

data Segment = A | B | C | D | E | F | G
  deriving stock (Show, Eq, Ord, Bounded, Enum)

type Digit = Set Segment

solveRow :: ([Digit], [Digit]) -> Int
solveRow (ds, cs) = checkSum (idDigits (toDigitsMap ds)) cs

checkSum :: Map Digit Int -> [Digit] -> Int
checkSum m = foldl' (\acc n -> 10 * acc + n) 0 . fmap (m !)

toDigitsMap :: [Digit] -> Map Int [Digit]
toDigitsMap = foldl' go (Map.fromList (zip [0 .. 9] (repeat [])))
  where
    go :: Map Int [Digit] -> Digit -> Map Int [Digit]
    go m d
      | length d == 2 = Map.insert 1 [d] m
      | length d == 3 = Map.insert 7 [d] m
      | length d == 4 = Map.insert 4 [d] m
      | length d == 5 = Map.insertWith (<>) 2 [d] . Map.insertWith (<>) 3 [d] . Map.insertWith (<>) 5 [d] $ m
      | length d == 6 = Map.insertWith (<>) 0 [d] . Map.insertWith (<>) 6 [d] . Map.insertWith (<>) 9 [d] $ m
      | otherwise = Map.insert 8 [d] m

idDigits :: Map Int [Digit] -> Map Digit Int
idDigits = Map.fromList . map (\(a, b) -> (b, a)) . Map.toList . fmap head . idAll
  where
    idAll, id2, id9, id53, id06 :: Map Int [Digit] -> Map Int [Digit]
    idAll = id06 . id53 . id9 . id2
    id2 m = case diffSegments 4 2 m of
      Nothing -> m
      Just s ->
        let d2 = fmap snd $ filter ((== 2) . fst) $ zip (map Set.size s) (m ! 2)
         in Map.adjust (\\ d2) 5 . Map.adjust (\\ d2) 3 . Map.insert 2 d2 $ m
    id9 m = case diffSegments 4 9 m of
      Nothing -> m
      Just s ->
        let d9 = fmap snd $ filter ((== 0) . fst) $ zip (map Set.size s) (m ! 9)
         in Map.adjust (\\ d9) 0 . Map.adjust (\\ d9) 6 . Map.insert 9 d9 $ m
    id53 m = case diffSegments 2 5 m of
      Nothing -> m
      Just s ->
        let d5 = fmap snd $ filter ((== 2) . fst) $ zip (map Set.size s) (m ! 5)
         in Map.adjust (\\ d5) 3 . Map.insert 5 d5 $ m
    id06 m = case diffSegments 7 0 m of
      Nothing -> m
      Just s ->
        let d0 = fmap snd $ filter ((== 0) . fst) $ zip (map Set.size s) (m ! 0)
         in Map.adjust (\\ d0) 6 . Map.insert 0 d0 $ m
    diffSegments :: Int -> Int -> Map Int [Digit] -> Maybe [Set Segment]
    diffSegments a b m
      | length ad == 1 = Just $ map (Set.difference (head ad)) bd
      | length bd == 1 = Just $ map (Set.difference (head bd)) ad
      | otherwise = Nothing
      where
        ad = m ! a
        bd = m ! b

lineP :: Parser ([Digit], [Digit])
lineP = do
  ss1 <- someTill digitP (string "| ")
  ss2 <- someTill digitP newline
  pure (ss1, ss2)

digitP :: Parser Digit
digitP = do
  cs <- some (oneOf ['a' .. 'g']) <* optional (char ' ')
  Set.fromList <$> traverse (maybeToP parseL) cs

parseL :: Char -> Maybe Segment
parseL = \case
  'a' -> Just A
  'b' -> Just B
  'c' -> Just C
  'd' -> Just D
  'e' -> Just E
  'f' -> Just F
  'g' -> Just G
  _ -> Nothing
