module AoC.Days.Day05Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day05
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 05)
    parse
    (solveA, 7436)
    (solveB, 21104)
