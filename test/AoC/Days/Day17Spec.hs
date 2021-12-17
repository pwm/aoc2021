module AoC.Days.Day17Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day17
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 17)
    parse
    (solveA, 10585)
    (solveB, 5247)
