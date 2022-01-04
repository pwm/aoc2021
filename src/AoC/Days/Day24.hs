module AoC.Days.Day24 where

import AoC.Lib.Parser
import AoC.Prelude hiding (many, oneOf)
import Control.Monad.Logic
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

parse :: String -> Maybe [Cmd]
parse = parseMaybe (many cmdP)

solveA :: [Cmd] -> Int
solveA _ = maximum solve

solveB :: [Cmd] -> Int
solveB _ = minimum solve

solve :: [Int]
solve = fmap (fromMaybe 0 . digitsToInt) $ observeAll $ go 0 [] mempty 0
  where
    go :: Int -> [Int] -> IntMap Int -> Int -> Logic [Int]
    go idx ws m z
      | idx == 14 && z == 0 = pure (reverse ws)
      | idx == 14 = empty
      | otherwise = do
        (z', w) <- choose $ [nextZwithW idx z w | w <- [1 .. 9]]
        -- human introspection of the mysterious MONAD restrictions
        -- revealed that values of z' must make the following pattern:
        -- idx :   0 1 2 3 4 5 6 7 8 9 0 1 2
        -- z'  : 0 a b c d _ d _ d c _ c b a 0
        guard (idx /= 5 || m ! 3 == z')
        guard (idx /= 7 || m ! 3 == z')
        guard (idx /= 8 || m ! 2 == z')
        guard (idx /= 10 || m ! 2 == z')
        guard (idx /= 11 || m ! 1 == z')
        guard (idx /= 12 || m ! 0 == z')
        go (idx + 1) (w : ws) (IntMap.insert idx z' m) z'

-- one full "digit cycle" in high-level form
nextZwithW :: Int -> Int -> Int -> (Int, Int)
nextZwithW idx z w =
  let (a, b, c) = abcs !! idx
      z' =
        if z `mod` 26 + b == w
          then z `div` a
          else (z `div` a) * 26 + w + c
   in (z', w)

-- specific values for each cycle of nextZwithW
abcs :: [(Int, Int, Int)]
abcs =
  [ (1, 13, 6),
    (1, 11, 11),
    (1, 12, 5),
    (1, 10, 6),
    (1, 14, 8),
    (26, -1, 14),
    (1, 14, 9),
    (26, -16, 4),
    (26, -8, 7),
    (1, 12, 13),
    (26, -16, 11),
    (26, -13, 11),
    (26, -6, 6),
    (26, -6, 1)
  ]

-- VM that runs the commands, only for introspection

data Cmd
  = Inp Reg Int
  | Add Reg Var
  | Mul Reg Var
  | Div Reg Var
  | Mod Reg Var
  | Eql Reg Var
  deriving stock (Show, Eq, Ord, Generic)

data Var
  = VReg Reg
  | VInt Int
  deriving stock (Show, Eq, Ord, Generic)

data Reg = W | X | Y | Z
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

type Regs = Map Reg Int

run :: [Cmd] -> Int -> Regs
run cmds is0 = fst $ execState (traverse_ step cmds) (initRegs, intToDigits is0)
  where
    step :: Cmd -> State (Regs, [Int]) ()
    step cmd = do
      (rs, is) <- get
      let (cmd', is') = case cmd of
            Inp r _ -> (Inp r (head is), tail is)
            c -> (c, is)
      put (eval rs cmd', is')

initRegs :: Regs
initRegs = Map.fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]

eval :: Regs -> Cmd -> Regs
eval rs cmd =
  let (k, i) = case cmd of
        Inp r n -> (r, n)
        Add r v -> (r, rs Map.! r + lkp rs v)
        Mul r v -> (r, rs Map.! r * lkp rs v)
        Div r v -> (r, rs Map.! r `div` lkp rs v)
        Mod r v -> (r, rs Map.! r `mod` lkp rs v)
        Eql r v -> (r, if rs Map.! r == lkp rs v then 1 else 0)
   in Map.insert k i rs

lkp :: Regs -> Var -> Int
lkp rs = \case
  VReg r -> rs Map.! r
  VInt n -> n

-- Parser

cmdP :: Parser Cmd
cmdP = do
  cmdTyp <- choice ["inp", "add", "mul", "div", "mod", "eql"] <* spaceChar
  v1 <- regP
  if cmdTyp == "inp"
    then pure $ Inp v1 0
    else do
      v2 <- try (VReg <$> regP) <|> (VInt <$> signedIntP)
      let parseCmd :: String -> Maybe Cmd
          parseCmd = \case
            "add" -> Just $ Add v1 v2
            "mul" -> Just $ Mul v1 v2
            "div" -> Just $ Div v1 v2
            "mod" -> Just $ Mod v1 v2
            "eql" -> Just $ Eql v1 v2
            _ -> Nothing
      maybeToP parseCmd cmdTyp

regP :: Parser Reg
regP = do
  let parseReg :: Char -> Maybe Reg
      parseReg = \case
        'w' -> Just W
        'x' -> Just X
        'y' -> Just Y
        'z' -> Just Z
        _ -> Nothing
  c <- oneOf ['w', 'x', 'y', 'z'] <* optional spaceChar
  maybeToP parseReg c
