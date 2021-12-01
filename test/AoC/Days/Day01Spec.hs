module AoC.Days.Day01Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day01
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 01)
    parse
    (solveA, 1_722)
    (solveB, 1_748)
