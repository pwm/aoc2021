module AoC.Days.Day20Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day20
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 20)
    parse
    (solveA, 5819)
    (solveB, 18516)
