module AoC.Days.Day21Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day21
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 21)
    parse
    (solveA, 713328)
    (solveB, 92399285032143)
