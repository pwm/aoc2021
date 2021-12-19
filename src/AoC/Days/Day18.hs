module AoC.Days.Day18 where

import AoC.Lib.Parser hiding (choice)
import AoC.Prelude hiding (split)
import Control.Monad.Logic

parse :: String -> Maybe [BinT Int]
parse = traverse (parseMaybe treeP) . lines

solveA :: [BinT Int] -> Int
solveA = magnitude . foldl1' add

solveB :: [BinT Int] -> Int
solveB =
  maximum
    . fmap (magnitude . uncurry add)
    . addFlippedPairs
    . fromMaybe []
    . traverse l2p
    . pick 2

data BinT a
  = Leaf a
  | Node (BinT a) (BinT a)
  deriving stock (Show, Eq, Functor)

pattern Pair :: a -> a -> BinT a
pattern Pair l r = Node (Leaf l) (Leaf r)

add :: BinT Int -> BinT Int -> BinT Int
add t1 t2 = focus . fixpoint reduce . mkFocus $ Node t1 t2
  where
    reduce :: Focus Int -> Focus Int
    reduce z@(Focus t _)
      | depth t > 4 = explode z
      | any (> 9) (leaves t) = split z
      | otherwise = z

explode, split :: Focus Int -> Focus Int
explode = root . explodeNode . leftmost exploding
split = root . splitLeaf . leftmost splitting

explodeNode :: Focus Int -> Focus Int
explodeNode = \case
  z@(Focus (Pair l r) _) -> addToLR l r $ putFocus (Leaf 0) z
  z -> z
  where
    -- Left fold over the contexts on the focus aka. walking up the tree
    -- In the 1st left subtree (if any) find the rightmost leaf and add the left number
    -- In the 1st right subtree (if any) find the leftmost leaf and add the right number
    -- Otherwise just keep folding up till the root
    addToLR :: Int -> Int -> Focus Int -> Focus Int
    addToLR l r (Focus t0 ctxs) =
      fst $ foldl' ctxStep (mkFocus t0, (False, False)) ctxs
      where
        ctxStep :: (Focus Int, (Bool, Bool)) -> Ctx Int -> (Focus Int, (Bool, Bool))
        ctxStep (Focus t _, (False, doneR)) (R lt) =
          let ctx = R . focus . root . mapFocus (+ l) . rightmost leaf . mkFocus $ lt
           in (up (Focus t [ctx]), (True, doneR))
        ctxStep (Focus t _, (doneL, False)) (L rt) =
          let ctx = L . focus . root . mapFocus (+ r) . leftmost leaf . mkFocus $ rt
           in (up (Focus t [ctx]), (doneL, True))
        ctxStep (Focus t _, b) ctx = (up (Focus t [ctx]), b)

splitLeaf :: Focus Int -> Focus Int
splitLeaf = modifyFocus splitIt
  where
    splitIt = \case
      Leaf n -> Pair (n `div` 2) ((n + 1) `div` 2)
      t -> t

exploding, splitting, leaf :: Focus Int -> Bool
exploding = \case
  (Focus (Pair _ _) ctxs) | length ctxs >= 4 -> True
  _ -> False
splitting = \case
  Focus (Leaf n) _ | n > 9 -> True
  _ -> False
leaf = \case
  (Focus (Leaf _) _) -> True
  _ -> False

leftmost, rightmost :: (Focus a -> Bool) -> Focus a -> Focus a
leftmost = search (\z -> [left z, right z])
rightmost = search (\z -> [right z, left z])

magnitude :: BinT Int -> Int
magnitude = \case
  Leaf n -> n
  Node lt rt -> 3 * magnitude lt + 2 * magnitude rt

depth :: BinT a -> Int
depth = \case
  Leaf _ -> 0
  Node lt rt -> 1 + depth lt `max` depth rt

leaves :: BinT a -> [a]
leaves = \case
  Leaf n -> [n]
  Node lt rt -> leaves lt <> leaves rt

addFlippedPairs :: [(a, a)] -> [(a, a)]
addFlippedPairs xs = fmap (\(a, b) -> (b, a)) xs <> xs

-- Tree search

search ::
  forall a.
  (Focus a -> [Focus a]) ->
  (Focus a -> Bool) ->
  Focus a ->
  Focus a
search candidates found z0 = headOr z0 . observeMany 1 . go $ z0
  where
    go :: Focus a -> Logic (Focus a)
    go = \case
      z | found z -> pure z
      Focus (Leaf _) _ -> empty
      z -> choose (candidates z) >>= go

-- Focus aka. Zipper

data Ctx a
  = L (BinT a)
  | R (BinT a)
  deriving stock (Show, Eq, Functor)

data Focus a = Focus
  { focus :: BinT a,
    contexts :: [Ctx a]
  }
  deriving stock (Show, Eq, Functor)

mkFocus :: BinT a -> Focus a
mkFocus t = Focus t []

mapFocus :: (a -> a) -> Focus a -> Focus a
mapFocus f (Focus t ctxs) = Focus (fmap f t) ctxs

modifyFocus :: (BinT a -> BinT a) -> Focus a -> Focus a
modifyFocus f (Focus t bs) = Focus (f t) bs

putFocus :: BinT a -> Focus a -> Focus a
putFocus t = modifyFocus (const t)

root :: (Eq a) => Focus a -> Focus a
root = fixpoint up

left, right, up :: Focus a -> Focus a
left z@(Focus (Leaf _) _) = z
left (Focus (Node lt rt) ctxs) = Focus lt (L rt : ctxs)
right z@(Focus (Leaf _) _) = z
right (Focus (Node lt rt) ctxs) = Focus rt (R lt : ctxs)
up (Focus t []) = Focus t []
up (Focus lt (L rt : ctxs)) = Focus (Node lt rt) ctxs
up (Focus rt (R lt : ctxs)) = Focus (Node lt rt) ctxs

-- Parser

treeP :: Parser (BinT Int)
treeP = do
  _ <- char '['
  lt <- Leaf <$> intP <|> treeP
  _ <- char ','
  rt <- Leaf <$> intP <|> treeP
  _ <- char ']'
  pure $ Node lt rt
