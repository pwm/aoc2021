module AoC.Days.Day22 where

import AoC.Lib.Load
import AoC.Lib.Parser
import AoC.Prelude hiding (many)
import Data.Maybe
import Data.Set qualified as Set

{-
s <- load 22
i = fromJust $ parse s
solveA i
solveB i
-}

parse :: String -> Maybe [(Bool, C3)]
parse = parseMaybe (many rowP)

solveA :: [(Bool, C3)] -> Int
solveA = Set.size . initCore . fmap toPointSet . coreCommands

solveB :: [(Bool, C3)] -> ()
solveB _ = ()

data P = P
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

data C3 = C3
  { rx :: (Int, Int),
    ry :: (Int, Int),
    rz :: (Int, Int)
  }
  deriving stock (Show, Eq, Ord, Generic)

initCore :: [(Bool, Set P)] -> Set P
initCore = foldl' g mempty
  where
    g :: Set P -> (Bool, Set P) -> Set P
    g acc (on, s)
      | on = acc `Set.union` s
      | otherwise = acc `Set.difference` s

toPointSet :: (Bool, C3) -> (Bool, Set P)
toPointSet (on, C3 {rx, ry, rz}) = (on, Set.fromList ps)
  where
    p2r (a, b) = [a .. b]
    ps = [P x y z | x <- p2r rx, y <- p2r ry, z <- p2r rz]

coreCommands :: [(Bool, C3)] -> [(Bool, C3)]
coreCommands =
  let f (lo, hi) = -50 <= lo && hi <= 50
   in filter (\(_, C3 {rx, ry, rz}) -> and $ f <$> [rx, ry, rz])

{-

foldl' g [] [cuboids]

g l c -> undefined
  if on
    l' = c:l
    filter (intersection)
    if int on the  off i:l else on i:l

Keeping a list of cuboids, starting with an empty list, and going through each
cuboid one by one.

If the cuboid is on, first, add that cuboid to the list,
then iterate through the list of other cuboids, for each of those, if it is on,
add the overlapping to the list and mark it as off, otherwise, add the overlapping
cuboid and mark it as on.

For off cuboid, just iterate through the list of existing
cuboids and add overlapping with similar logic.

Finally, with the list, add the
volume of the cuboid to the result if it is an on cuboid, otherwise, subtract it.

      12345678901234567890
      aaaaaaaaaaaa         - 12
          bbbbbbbbbbbb     - 12 + 12 - 8(b|a) = 16
      cccccccccccccccccc   - 16 + 18 - 12(c|b) - 12(c|a) + 8(b|a) = 18
    dddddddddddddddddddd   - 18 + 20 - 18(d|c) - 12(d|b) - 12(d|a) + 12(c|b) + 12(c|a) = 20
      eeee                 - 20 + 4 - 4(e|d) - 4(e|c) - 0(e|b) - 4(e|a) +

-}

allCube :: [C3] -> (Int, [C3])
allCube = foldl' g (0, [])
  where
    g :: (Int, [C3]) -> C3 -> (Int, [C3])
    g (res, vs) v
      | True = (res + volume v - intersectionVolumes v vs, v : vs)
      | otherwise = (res - volume (undefined vs v), v : vs)

intersectionVolumes :: C3 -> [C3] -> Int
intersectionVolumes v0 = sum . fmap volume . intersections v0

intersections :: C3 -> [C3] -> [C3]
intersections v0 = mapMaybe (intersection v0)

intersection :: C3 -> C3 -> Maybe C3
intersection a b =
  if rx_lo <= rx_hi && ry_lo <= ry_hi && rz_lo <= rz_hi
    then Just $ C3 (rx_lo, rx_hi) (ry_lo, ry_hi) (rz_lo, rz_hi)
    else Nothing
  where
    rx_lo = max (fst (rx a)) (fst (rx b))
    rx_hi = min (snd (rx a)) (snd (rx b))
    ry_lo = max (fst (ry a)) (fst (ry b))
    ry_hi = min (snd (ry a)) (snd (ry b))
    rz_lo = max (fst (rz a)) (fst (rz b))
    rz_hi = min (snd (rz a)) (snd (rz b))

-- can be negative
edgeX, edgeY, edgeZ :: C3 -> Int
edgeX C3 {rx} = snd rx - fst rx + 1
edgeY C3 {ry} = snd ry - fst ry + 1
edgeZ C3 {rz} = snd rz - fst rz + 1

volume :: C3 -> Int
volume v = max 0 (edgeX v * edgeY v * edgeZ v)

--

rowP :: Parser (Bool, C3)
rowP = do
  let parseOnOff :: String -> Bool
      parseOnOff = \case
        "on" -> True
        _ -> False
  s <- parseOnOff <$> choice ["on", "off"] <* space
  x1 <- string "x=" *> signedIntP
  x2 <- string ".." *> signedIntP <* ","
  y1 <- string "y=" *> signedIntP
  y2 <- string ".." *> signedIntP <* ","
  z1 <- string "z=" *> signedIntP
  z2 <- string ".." *> signedIntP
  pure (s, C3 (x1, x2) (y1, y2) (z1, z2))
