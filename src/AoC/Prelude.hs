{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module AoC.Prelude
  ( module X,
    pp,
    printError,
    printSuccess,
    fixpoint,
    fixpointM,
    loopTill,
    loopTillM,
    headOr,
    dropEnd,
    enumerate,
    hasKeys,
    rsort,
    charAt,
    l2p,
    getDataFileName,
    pick,
    slicesOf,
    lookups,
    compose,
    composeM,
    times,
    timesL,
    timesM,
    substring,
    tupleMin,
    tupleMax,
    tupleSum,
    tupleProduct,
    binToDec,
    sqrtInt,
    choose,
  )
where

import Control.Applicative as X (Alternative (..), liftA2)
import Control.Lens as X (Each (..), element, over, set, toListOf, use, uses, view, (%=), (%~), (&), (.=), (.~), (^.), _1, _2, _3, _4, _5)
import Control.Monad as X (foldM, guard, when, (<=<), (>=>))
import Data.Bifunctor as X (first)
import Data.Either as X
import Data.Foldable as X (Foldable (..), asum)
import Data.Functor.Identity as X (Identity (..))
import Data.Generics.Labels as X ()
import Data.List as X
import Data.List.Split as X
import Data.Map.Strict as X (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe as X hiding (fromJust)
import Data.Ord as X (Down (..), comparing)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Set qualified as Set
import Data.Text as X (Text)
import Data.Void as X (Void)
import GHC.Generics as X (Generic)
import Paths_aoc2021 (getDataFileName)
import System.Exit (exitFailure, exitSuccess)
import System.IO as X (stdin)
import Text.Pretty.Simple (CheckColorTty (..), OutputOptions (..), StringOutputStyle (..), pPrintOpt)
import Prelude as X

pp :: (Show a) => a -> IO ()
pp =
  pPrintOpt
    NoCheckColorTty
    ( OutputOptions
        { outputOptionsIndentAmount = 2,
          outputOptionsPageWidth = 120,
          outputOptionsCompact = True,
          outputOptionsCompactParens = True,
          outputOptionsInitialIndent = 0,
          outputOptionsColorOptions = Nothing,
          outputOptionsStringStyle = EscapeNonPrintable
        }
    )

printError, printSuccess :: String -> IO ()
printError e = print e >> exitFailure
printSuccess v = print v >> exitSuccess

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x = if x == f x then x else fixpoint f (f x)

fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f x = do
  y <- f x
  if x == y then pure y else fixpointM f y

compose :: (Foldable t) => t (b -> b) -> b -> b
compose = foldr (.) id

composeM :: (Foldable t, Monad m) => t (b -> m b) -> b -> m b
composeM = foldr (<=<) pure

-- strict
times :: Int -> (b -> b) -> b -> b
times n f s = foldl' (\x _ -> f x) s (replicate n ())

-- lazy
timesL :: Int -> (b -> b) -> b -> b
timesL n = compose . replicate n

timesM :: (Monad m) => Int -> (b -> m b) -> b -> m b
timesM n = composeM . replicate n

loopTill :: (a -> Bool) -> (a -> a) -> a -> a
loopTill p step x =
  if p x then x else loopTill p step (step x)

loopTillM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
loopTillM p step x = do
  b <- p x
  if b then pure x else step x >>= loopTillM p step

-- headOr 0 [] -> 0
-- headOr 0 [1, 2, 3] -> 1
headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (a : _) = a

-- dropEnd 1 [1..3] -> [1, 2]
dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

-- enumerate @Bool -> [False,True]
enumerate :: forall a. (Bounded a, Enum a) => [a]
enumerate = enumFrom (minBound @a)

-- {a, b} -> [(a, 1), (b, 2), (c, 3)] -> True
-- {a, b, c} -> [(a, 1), (b, 2)] -> False
hasKeys :: (Ord a) => Set a -> Map a b -> Bool
hasKeys keys = Set.isSubsetOf keys . Map.keysSet

-- [2,3,1,2] -> [3,2,2,1]
rsort :: (Ord a) => [a] -> [a]
rsort = sortOn Down

-- 2 "abcd" -> Just 'c'
-- 5 "abcd" -> Nothing
charAt :: Int -> String -> Maybe Char
charAt x = fmap fst . uncons . drop x

-- [1, 2] -> Just (1, 2)
-- [1, 2, 3] -> Nothing
l2p :: [a] -> Maybe (a, a)
l2p [a, b] = Just (a, b)
l2p _ = Nothing

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x : xs) = fmap (x :) (pick (k - 1) xs) <> pick k xs

slicesOf :: Int -> [a] -> [[a]]
slicesOf n = unfoldr $ \xs ->
  let (s, t) = (take n xs, drop 1 xs)
   in if length s >= n then Just (s, t) else Nothing

lookups :: (Ord k) => Map k v -> [k] -> [v]
lookups g = mapMaybe (g !?)

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

tupleMin, tupleMax :: (Ord a, Each s s a a) => s -> a
tupleMin = minimum . toListOf each
tupleMax = maximum . toListOf each

tupleSum, tupleProduct :: (Num a, Each s s a a) => s -> a
tupleSum = sum . toListOf each
tupleProduct = product . toListOf each

binToDec :: [Bool] -> Integer
binToDec = foldl' (\acc x -> 2 * acc + toInteger (fromEnum x)) 0

sqrtInt :: Int -> Int
sqrtInt = floor @Double . sqrt . fromIntegral

choose :: (Traversable t, Alternative m) => t a -> m a
choose = asum . fmap pure
