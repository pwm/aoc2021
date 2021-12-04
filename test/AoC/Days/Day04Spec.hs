module AoC.Days.Day04Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day04
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 04)
    parse
    (solveA, 71708)
    (solveB, 34726)
