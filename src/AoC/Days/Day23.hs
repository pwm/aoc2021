module AoC.Days.Day23 where

import AoC.Lib.Graph
import AoC.Lib.Grid hiding (move)
import AoC.Prelude
import Control.Monad.Logic
import Control.Monad.State.Strict
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Board
parse = fmap insideBoard . parseGrid parseCell

solveA :: Board -> Int
solveA = view (_2 . #minSpent) . solve

solveB :: Board -> Int
solveB = view (_2 . #minSpent) . solve . extend

type Board = GridOf (Maybe Amphipod)

data Game = Game
  { board :: Board,
    spent :: Int,
    moves :: [Move]
  }
  deriving stock (Show, Eq, Generic)

data Move = Move
  { from :: Pos,
    to :: Pos,
    amphipod :: Amphipod,
    steps :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

data Amphipod = Amphipod
  { kind :: Kind,
    status :: Status
  }
  deriving stock (Show, Eq, Ord, Generic)

data Kind = Amber | Bronze | Copper | Desert
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

data Status = Guest | Hallway | Home
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

type Rooms = Map Kind Room

data Room = Room
  { status :: RoomStatus,
    nextPos :: Maybe Pos
  }
  deriving stock (Show, Eq, Ord, Generic)

data RoomStatus = Closed | Open | Done
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

data Inaccessible = Wall | Outside
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

data GameState = GameState
  { minSpent :: Int,
    reached :: Map Board Int,
    i :: Int
  }
  deriving stock (Show, Generic)

newtype Solver s a = Solver {unwrap :: LogicT (State s) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadPlus,
      MonadLogic,
      MonadState s
    )

solve :: Board -> ([Game], GameState)
-- solve b =
--   withIO (displayMoves bestMoves b)
--     . withIO (print $ gs ^. #i)
--     $ (games, gs)
--   where
--     (games, gs) = runSolver (GameState maxBound mempty 0) . solver . mkGame $ b
--     bestMoves = fromMaybe [] $ minimumByOf folded (comparing (view #spent)) games ^? _Just . #moves
solve = runSolver (GameState maxBound mempty 0) . solver . mkGame

runSolver :: s -> Solver s a -> ([a], s)
runSolver gs = flip runState gs . observeAllT . unwrap

mkGame :: Board -> Game
mkGame board = Game board 0 []

{-
The goal is to solve the board in all possible ways and then select the
solution with the minimum energy spent. As the search space is huge we
must employ reduction strategies it in order to make this viable.

The two different early cut-off strategies are:
  1. Already spent more energy than the current minimum solved board
  2. Already reached this board state with less energy spent

In both cases we can safely abort subsequent search.

The 3rd way to reduce search is reducing the set of potential next moves:
  3. If an amphipod can go home make that the only choice as it will have
     to go home anyway.

This reduces the set of next moves to one.
-}
solver :: Game -> Solver GameState Game
solver game@Game {board, spent} = do
  #i %= (+ 1)
  minSpent <- use #minSpent
  reached <- use #reached
  if
      | spent >= minSpent -> empty
      | spent >= fromMaybe maxBound (reached !? board) -> empty
      | solved board -> do
        #minSpent .= spent
        pure game
      | otherwise -> do
        #reached %= Map.insert board spent
        move <- choose $ candidateMoves board
        solver $
          game
            & #board %~ update move
            & #spent +~ cost move
            & #moves %~ (move :)

solved :: Board -> Bool
solved b = checkRooms b ^.. each . #status == [Done, Done, Done, Done]

candidateMoves :: Board -> [Move]
candidateMoves b = case homeRun adjMap rooms b of
  Nothing -> hallwayRun adjMap b
  Just m -> [m]
  where
    adjMap :: GridOf [Pos] = toAdjMap b
    rooms :: Rooms = checkRooms b

update :: Move -> Board -> Board
update Move {from, to, amphipod} =
  Map.insert to (Just amphipod') . Map.insert from Nothing
  where
    amphipod' = amphipod & #status .~ transition to
    transition :: Pos -> Status
    transition (1, _) = Hallway
    transition _ = Home

cost :: Move -> Int
cost m = m ^. #steps * energy (m ^. #amphipod . #kind)

-- Finding good moves

homeRun :: GridOf [Pos] -> Rooms -> Board -> Maybe Move
homeRun adjMap rooms0 b
  | null hrs = Nothing
  | otherwise = case mapMaybe (tracePath adjMap) hrs of
    [] -> Nothing
    mos -> Just $ toMove b (head mos) -- ok to just pick one homerun
  where
    hrs :: [(Pos, Pos)] = readyPodsToOpenRooms rooms0 b
    readyPodsToOpenRooms :: Rooms -> Board -> [(Pos, Pos)]
    readyPodsToOpenRooms rooms = mapMaybe (bisequenceA . f) . readyPods
      where
        f :: (Pos, Amphipod) -> (Maybe Pos, Maybe Pos)
        f (pos, a) = (Just pos, (openRooms rooms !? (a ^. #kind)) ^? _Just . #nextPos . _Just)
        openRooms :: Rooms -> Rooms
        openRooms = Map.filter ((== Open) . view #status)

hallwayRun :: GridOf [Pos] -> Board -> [Move]
hallwayRun adjMap b = fmap (toMove b) . mapMaybe (tracePath adjMap) $ fromTos
  where
    fromTos :: [(Pos, Pos)] = (,) <$> readyPodsInRooms b <*> hallway
    readyPodsInRooms :: Board -> [Pos]
    readyPodsInRooms = map fst . filter (\((r, _), _) -> r /= 1) . readyPods

readyPods :: Board -> [(Pos, Amphipod)]
readyPods b =
  Map.toList
    . Map.filterWithKey (\p _ -> not $ null $ emptyNeighbours b p)
    . Map.filter (\a -> a ^. #status /= Home)
    . Map.mapMaybe id
    $ b

toAdjMap :: Board -> GridOf [Pos]
toAdjMap b =
  Map.filter (/= []) $
    Map.foldrWithKey (\p _ m -> Map.insert p (emptyNeighbours b p) m) mempty b

tracePath :: GridOf [Pos] -> (Pos, Pos) -> Maybe [Pos]
tracePath adjMap (from, to)
  | Map.notMember from adjMap = Nothing
  | otherwise =
    let path = dfs nexts (== to) from
     in if null path || last path /= to
          then Nothing
          else Just path
  where
    -- we only ever pick a single next best step based on manhattan distance from the target
    -- and a tiebreaker if 2 next steps have the same manhattan distance
    nexts :: Pos -> [Pos]
    nexts = take 1 . filter tiebreaker . decoSort (manhattan to) . fromMaybe [] . (adjMap !?)
    -- stay in the hallway, ie. don't go into a wrong room
    tiebreaker :: Pos -> Bool
    tiebreaker (x, y)
      | fst to > 1 && x > 1 = snd to == y
      | otherwise = True

toMove :: Board -> [Pos] -> Move
toMove b ps =
  Move
    { from = head ps,
      to = last ps,
      amphipod = Map.mapMaybe id b ! head ps,
      steps = length ps - 1
    }

-- Util

extend :: Board -> Board
extend b =
  Map.insert (3, room Amber) (addGuest Desert)
    . Map.insert (3, room Bronze) (addGuest Copper)
    . Map.insert (3, room Copper) (addGuest Bronze)
    . Map.insert (3, room Desert) (addGuest Amber)
    . Map.insert (4, room Amber) (addGuest Desert)
    . Map.insert (4, room Bronze) (addGuest Bronze)
    . Map.insert (4, room Copper) (addGuest Amber)
    . Map.insert (4, room Desert) (addGuest Copper)
    . Map.insert (5, room Amber) (b ! (3, room Amber))
    . Map.insert (5, room Bronze) (b ! (3, room Bronze))
    . Map.insert (5, room Copper) (b ! (3, room Copper))
    . Map.insert (5, room Desert) (b ! (3, room Desert))
    $ b
  where
    addGuest :: Kind -> Maybe Amphipod
    addGuest k = Just (Amphipod k Guest)

checkRooms :: Board -> Rooms
checkRooms b =
  Map.fromList
    [ (Amber, checkRoom Amber),
      (Bronze, checkRoom Bronze),
      (Copper, checkRoom Copper),
      (Desert, checkRoom Desert)
    ]
  where
    checkRoom :: Kind -> Room
    checkRoom k
      | x2 == [Nothing] || x2 == [Nothing, Just Home] = Room Open (Just (row, room k))
      | x2 == [Just Home] = Room Done Nothing
      | otherwise = Room Closed Nothing
      where
        x1 = Map.filterWithKey (\(r, c) _ -> r >= 2 && c == room k) b
        x2 = nubOrd $ map (fmap (view #status)) $ Map.elems x1
        row = fst $ fst $ Map.findMax $ Map.filter isNothing x1

emptyNeighbours :: Board -> Pos -> [Pos]
emptyNeighbours b = mapMaybe lookupEmpty . adj4
  where
    lookupEmpty :: Pos -> Maybe Pos
    lookupEmpty p = case b !? p of
      Just Nothing -> Just p
      _ -> Nothing

room :: Kind -> Int
room = \case
  Amber -> 3
  Bronze -> 5
  Copper -> 7
  Desert -> 9

energy :: Kind -> Int
energy = \case
  Amber -> 1
  Bronze -> 10
  Copper -> 100
  Desert -> 1000

hallway, doorSteps :: [Pos]
hallway = [(1, 1), (1, 2), (1, 4), (1, 6), (1, 8), (1, 10), (1, 11)]
doorSteps = [(1, 3), (1, 5), (1, 7), (1, 9)]

-- Parser

parseCell :: Char -> Maybe (Either Inaccessible (Maybe Kind))
parseCell = \case
  '.' -> Just (Right Nothing)
  'A' -> Just (Right (Just Amber))
  'B' -> Just (Right (Just Bronze))
  'C' -> Just (Right (Just Copper))
  'D' -> Just (Right (Just Desert))
  '#' -> Just (Left Wall)
  ' ' -> Just (Left Outside)
  _ -> Nothing

insideBoard :: GridOf (Either Inaccessible (Maybe Kind)) -> Board
insideBoard = Map.mapMaybe insideCell
  where
    insideCell :: Either Inaccessible (Maybe Kind) -> Maybe (Maybe Amphipod)
    insideCell = \case
      Right (Just k) -> Just (Just (Amphipod k Guest))
      Right Nothing -> Just Nothing
      Left _ -> Nothing

-- Printer

displayMoves :: [Move] -> Board -> IO ()
displayMoves ms b0 = foldM_ f b0 (reverse ms)
  where
    f :: Board -> Move -> IO Board
    f b m = let b' = update m b in putStrLn (printBoard b') >> pure b'

printBoard :: Board -> String
printBoard = (<> "\n") . printGrid printCell . toFullBoard

toFullBoard :: Board -> GridOf (Either Inaccessible (Maybe Kind))
toFullBoard b = Map.map f b <> (outside <> walls)
  where
    outside = Map.fromList $ zip ((,) <$> [3 .. 6] <*> [0, 1, 11, 12]) (repeat (Left Outside))
    walls = Map.fromList $ zip ((,) <$> [0 .. 6] <*> [0 .. 12]) (repeat (Left Wall))
    f :: Maybe Amphipod -> Either Inaccessible (Maybe Kind)
    f Nothing = Right Nothing
    f (Just (Amphipod k _)) = Right (Just k)

printCell :: Either Inaccessible (Maybe Kind) -> Char
printCell = \case
  Left Wall -> '#'
  Left Outside -> ' '
  Right (Just Amber) -> 'A'
  Right (Just Bronze) -> 'B'
  Right (Just Copper) -> 'C'
  Right (Just Desert) -> 'D'
  Right Nothing -> '.'
