module AoC.Days.Day15Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day15
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 15)
    parse
    (solveA, 702)
    (solveB, 2955)
