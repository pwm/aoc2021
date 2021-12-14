module AoC.Days.Day14Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day14
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 14)
    parse
    (solveA, 3230)
    (solveB, 3542388214529)
