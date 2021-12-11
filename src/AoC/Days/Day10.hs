module AoC.Days.Day10 where

import AoC.Prelude

parse :: String -> Maybe [[Bracket]]
parse = traverse (traverse parseBracket) . lines

solveA :: [[Bracket]] -> Int
solveA =
  sum
    . mapMaybe (fmap (scoreA . strip) . view #cur)
    . filter (not . view #valid)
    . map processBrackets

solveB :: [[Bracket]] -> Int
solveB =
  middle
    . sort
    . map (scoresB . map (strip . pair) . view #stack)
    . filter (view #valid)
    . map processBrackets
  where
    middle l = head $ take 1 $ drop (length l `div` 2) l

data Bracket
  = Opening BracketType
  | Closing BracketType
  deriving stock (Show)

data BracketType
  = Paren
  | Square
  | Curly
  | Angle
  deriving stock (Show, Eq, Bounded, Enum)

data Env = MkEnv
  { cur :: Maybe Bracket,
    stack :: [Bracket],
    valid :: Bool
  }
  deriving stock (Show, Generic)

newEnv :: Env
newEnv = MkEnv Nothing [] True

processBrackets :: [Bracket] -> Env
processBrackets = go newEnv
  where
    go :: Env -> [Bracket] -> Env
    go env [] = env
    go env@(MkEnv _ _ False) _ = env
    go env (b : bs) = go (processBracket b env) bs

processBracket :: Bracket -> Env -> Env
processBracket b (MkEnv _ s _) = case b of
  Opening _ -> MkEnv (Just b) (b : s) True
  Closing _ ->
    let (mob, s') = pop s
        valid = case mob of
          Just ob -> strip ob == strip b
          Nothing -> False
     in MkEnv (Just b) s' valid

strip :: Bracket -> BracketType
strip (Opening bt) = bt
strip (Closing bt) = bt

pair :: Bracket -> Bracket
pair (Opening b) = Closing b
pair (Closing b) = Opening b

pop :: [a] -> (Maybe a, [a])
pop [] = (Nothing, [])
pop (x : xs) = (Just x, xs)

scoreA :: BracketType -> Int
scoreA = \case
  Paren -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137

scoreB :: BracketType -> Int
scoreB = \case
  Paren -> 1
  Square -> 2
  Curly -> 3
  Angle -> 4

scoresB :: [BracketType] -> Int
scoresB = foldl' (\acc v -> 5 * acc + scoreB v) 0

parseBracket :: Char -> Maybe Bracket
parseBracket = \case
  '(' -> Just $ Opening Paren
  '[' -> Just $ Opening Square
  '{' -> Just $ Opening Curly
  '<' -> Just $ Opening Angle
  ')' -> Just $ Closing Paren
  ']' -> Just $ Closing Square
  '}' -> Just $ Closing Curly
  '>' -> Just $ Closing Angle
  _ -> Nothing
