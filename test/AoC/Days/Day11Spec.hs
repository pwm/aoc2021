module AoC.Days.Day11Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day11
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 11)
    parse
    (solveA, 1594)
    (solveB, 437)
