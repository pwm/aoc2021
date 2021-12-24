module AoC.Days.Day23Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day23
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 23)
    parse
    (solveA, 10411)
    (solveB, 46721)
