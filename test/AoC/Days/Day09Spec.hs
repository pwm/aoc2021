module AoC.Days.Day09Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day09
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 09)
    parse
    (solveA, 580)
    (solveB, 856716)
