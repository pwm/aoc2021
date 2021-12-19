module AoC.Days.Day18Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day18
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 18)
    parse
    (solveA, 3816)
    (solveB, 4819)
