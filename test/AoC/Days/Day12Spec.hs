module AoC.Days.Day12Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day12
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 12)
    parse
    (solveA, 5958)
    (solveB, 150426)
