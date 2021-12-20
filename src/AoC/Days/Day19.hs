module AoC.Days.Day19 where

import AoC.Lib.Load
import AoC.Lib.Parser
import AoC.Prelude hiding (sepBy, some)
import Data.Maybe

{-
s <- load 19
i = fromJust $ parse s
solveA i
-}
parse :: String -> Maybe [[XYZ]]
parse = parseMaybe (scannerP `sepBy` newline)

solveA :: [[XYZ]] -> ()
solveA _ = ()

solveB :: [[XYZ]] -> ()
solveB _ = ()

type XYZ = (Int, Int, Int)

{-
finding pairs of scanners that have overlapping detection regions such that 
there are at least 12 beacons that both scanners detect within the overlap

By establishing 12 common beacons, you can precisely determine where the scanners 
are relative to each other, allowing you to reconstruct the beacon map one scanner 
at a time.

the scanners also don't know their rotation or facing direction. 
Due to magnetic alignment, each scanner is rotated some integer number of 90-degree 
turns around all of the x, y, and z axes.

In total, each scanner could be in any of 24 different orientations: 
facing positive or negative x, y, or z, and considering any of four directions "up" 
from that facing.
-}

scannerP :: Parser [XYZ]
scannerP = do
  _ <- string "--- scanner " <* intP <* string "---" <* newline
  endBy1 xyzP eol

xyzP :: Parser XYZ
xyzP = do
  x <- signedIntP <* char ','
  y <- signedIntP <* char ','
  z <- signedIntP0
  pure (x, y, z)
